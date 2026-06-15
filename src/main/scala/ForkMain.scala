package ortholattices
import ortholattices.algorithms.*
import Datastructures.*
import Benchmark.{Algo, Improvement}

/** Command-line entry point for the ortholattices library.
 *
 * Available commands:
 *
 *   `<ol|ocbsl|structural> <aig-path>`
 *     Normalise all outputs of an AIGER circuit with the chosen algorithm.
 *     Prints size before/after, compression ratio, and wall-clock time.
 *
 *   `sizes <epfl-folder>`
 *     Print the AND-gate count for each EPFL arithmetic benchmark circuit.
 *
 *   `epfl <epfl-folder> [timeoutMinutes]`
 *     Run the full EPFL normalisation benchmark (default timeout: 5 min).
 *
 *   `entail <validity|reflexivity> <aig-path>`
 *     For each output bit, check OL entailment (⊤ ≤ φ or φ ≤ φ) without
 *     Tseitin axioms.  Prints `RESULT n_out n_valid time_ms`.
 *
 *   `entail-tseitin <mode> <aig-path>`
 *     Same as `entail` but injects Tseitin circuit axioms.  Stops at the
 *     first proof.  Supports the additional mode `equiv-renamed`.
 *
 *   `entail-tseitin-all <mode> <aig-path>`
 *     Like `entail-tseitin` but drains the full worklist and also reports
 *     the total number of proven sub-goals and sub-formulas examined.
 *     Prints `RESULT n_out n_valid n_proven n_sf time_ms`.
 *
 *   `verify [<mode>] <aig-path> <valid-indices-file>`
 *     Verify that the output bits listed in the indices file are actually
 *     OL-valid (default mode: `validity`).
 *
 *   `check-equiv <ol|ocbsl|structural> <aig-path> <bit-index>`
 *     Sanity-check a single output bit: run the normaliser, then test OL
 *     entailment in both directions with and without Tseitin axioms.
 *
 *   `random-bench <n> <size> <vars> <folder> [-check] [-nocbsl] [-nol]`
 *     Generate `n` random formulas (up to `size` nodes, `vars` variables), normalise
 *     with OL/OCBSL, and write results to `<folder>/results/random.txt`.
 */
object ForkMain {

  // ── dispatch ───────────────────────────────────────────────────────────────

  def main(args: Array[String]): Unit = args match {
    case Array("sizes", folder)                           => runSizes(folder)
    case Array("epfl", folder)                            => runEpfl(folder)
    case Array("epfl", folder, timeoutMin)                => runEpfl(folder, timeoutMin.toLong)
    case Array("entail", mode, path)                      => runEntail(mode, path)
    case Array("entail-tseitin", mode, path)              => runEntailTseitin(mode, path)
    case Array("entail-tseitin-all", mode, path)          => runEntailTseitinAll(mode, path)
    case Array("verify", aigPath, validIndicesPath)       => runVerify("validity", aigPath, validIndicesPath)
    case Array("verify", mode, aigPath, validIndicesPath) => runVerify(mode, aigPath, validIndicesPath)
    case Array("check-equiv", algoStr, aigPath, bitStr)   => runCheckEquiv(algoStr, aigPath, bitStr.toInt)
    case Array("random-bench", numberStr, sizeStr, varsStr, folder) =>
      runRandomBench(numberStr.toInt, sizeStr.toInt, varsStr.toInt, folder)
    case Array("random-bench", numberStr, sizeStr, varsStr, folder, flags @ _*) =>
      runRandomBench(numberStr.toInt, sizeStr.toInt, varsStr.toInt, folder,
        check  = flags.contains("-check"),
        ocbsl  = !flags.contains("-nocbsl"),
        ol     = !flags.contains("-nol"))
    case Array(algoStr, path)                             => runNormalize(algoStr, path)
    case _ =>
      println(
        """|Usage:
           |  ForkMain <ol|ocbsl|structural> <aig-path>
           |  ForkMain sizes <epfl-folder>
           |  ForkMain epfl <epfl-folder> [timeoutMinutes]
           |  ForkMain entail <validity|reflexivity> <aig-path>
           |  ForkMain entail-tseitin <mode> <aig-path>
           |  ForkMain entail-tseitin-all <mode> <aig-path>
           |  ForkMain verify [<mode>] <aig-path> <valid-indices-file>
           |  ForkMain check-equiv <ol|ocbsl|structural> <aig-path> <bit-index>
           |  ForkMain random-bench <n> <size> <vars> <folder> [-check] [-nocbsl] [-nol]
           |""".stripMargin)
      sys.exit(1)
  }

  // ── commands ───────────────────────────────────────────────────────────────

  /** Print AND-gate counts for the 10 EPFL arithmetic benchmark circuits. */
  def runSizes(folder: String): Unit =
    val cases = List("adder", "bar", "div", "hyp", "log2", "max", "multiplier", "sin", "sqrt", "square")
    cases.foreach { c =>
      val formulas = AigerParser.getAigerFormulas(s"$folder/$c.aig")
      println(s"$c: ${circuitSize(formulas)}")
    }

  /** Run the full EPFL normalisation benchmark with the given timeout (minutes, default 5). */
  def runEpfl(folder: String, timeoutMin: Long = 5): Unit =
    Benchmark.epflAigerBenchmark(folder + "/", scala.concurrent.duration.Duration(timeoutMin, "min"))

  /** For each output bit, check OL entailment without Tseitin axioms.
   *  Prints `RESULT n_out n_valid time_ms`. */
  def runEntail(mode: String, path: String): Unit =
    val formulas = AigerParser.getAigerFormulas(path).toIndexedSeq
    val t0 = System.nanoTime()
    var nValid = 0
    formulas.foreach { f =>
      val (lhs, rhs) = problemForMode(mode, f)
      if EntailmentAlgorithm.isEntailed(lhs, rhs) then nValid += 1
    }
    val elapsedMs = (System.nanoTime() - t0) / 1_000_000
    println(s"RESULT ${formulas.length} $nValid $elapsedMs")

  /** For each output bit, check OL entailment with Tseitin circuit axioms.
   *  Stops at the first proof per output.  Supports the extra mode `equiv-renamed`.
   *  Prints `RESULT n_out n_valid time_ms`. */
  def runEntailTseitin(mode: String, path: String): Unit =
    val (formulas, nInputs) = AigerParser.getAigerFormulasWithInfo(path)
    val startVar = 2 * (nInputs + 1)
    val t0 = System.nanoTime()
    var nValid = 0
    if mode == "equiv-renamed" then
      val (roots1, axioms1, nextVar) = AigerParser.tseitinAxioms(formulas, startVar)
      val (roots2, axioms2, _)       = AigerParser.tseitinAxioms(formulas, nextVar)
      val axioms = axioms1 ++ axioms2
      roots1.zip(roots2).foreach { case (z, z2) =>
        if EntailmentAlgorithm.isEntailed(z, z2, axioms) then nValid += 1
      }
    else
      val (roots, axioms, _) = AigerParser.tseitinAxioms(formulas, startVar)
      roots.foreach { root =>
        val (lhs, rhs) = problemForMode(mode, root)
        if EntailmentAlgorithm.isEntailed(lhs, rhs, axioms) then nValid += 1
      }
    val elapsedMs = (System.nanoTime() - t0) / 1_000_000
    println(s"RESULT ${formulas.length} $nValid $elapsedMs")

  /** Like [[runEntailTseitin]] but drains the full worklist.
   *  Prints `RESULT n_out n_valid n_proven n_sf time_ms`. */
  def runEntailTseitinAll(mode: String, path: String): Unit =
    val (formulas, nInputs) = AigerParser.getAigerFormulasWithInfo(path)
    val startVar = 2 * (nInputs + 1)
    val t0 = System.nanoTime()
    var nValid       = 0
    var nProvenTotal = 0L
    var nSfTotal     = 0L
    if mode == "equiv-renamed" then
      val (roots1, axioms1, nextVar) = AigerParser.tseitinAxioms(formulas, startVar)
      val (roots2, axioms2, _)       = AigerParser.tseitinAxioms(formulas, nextVar)
      val axioms = axioms1 ++ axioms2
      roots1.zip(roots2).foreach { case (z, z2) =>
        val stats = EntailmentAlgorithm.entailWithStats(z, z2, axioms)
        if stats.proved then nValid += 1
        nProvenTotal += stats.nProven
        nSfTotal     += stats.nFormulas
      }
    else
      val (roots, axioms, _) = AigerParser.tseitinAxioms(formulas, startVar)
      roots.foreach { root =>
        val (lhs, rhs) = problemForMode(mode, root)
        val stats = EntailmentAlgorithm.entailWithStats(lhs, rhs, axioms)
        if stats.proved then nValid += 1
        nProvenTotal += stats.nProven
        nSfTotal     += stats.nFormulas
      }
    val elapsedMs = (System.nanoTime() - t0) / 1_000_000
    println(s"RESULT ${formulas.length} $nValid $nProvenTotal $nSfTotal $elapsedMs")

  /** Verify that the output bits listed in `validIndicesPath` are OL-valid.
   *  Prints `VERIFIED` or `MISMATCH` with per-bit details. */
  def runVerify(mode: String, aigPath: String, validIndicesPath: String): Unit =
    val formulas = AigerParser.getAigerFormulas(aigPath).toIndexedSeq
    val source = scala.io.Source.fromFile(validIndicesPath)
    val claimedValid =
      try source.getLines().map(_.trim).filter(_.nonEmpty).map(_.toInt).toList
      finally source.close()
    val t0 = System.nanoTime()
    var confirmed = 0
    val mismatches = scala.collection.mutable.ListBuffer[Int]()
    claimedValid.foreach { i =>
      if i >= 0 && i < formulas.length then
        val (lhs, rhs) = problemForMode(mode, formulas(i))
        if EntailmentAlgorithm.isEntailed(lhs, rhs) then confirmed += 1
        else mismatches += i
      else mismatches += i
    }
    val elapsedMs = (System.nanoTime() - t0) / 1_000_000
    val total = claimedValid.size
    if mismatches.isEmpty then
      println(s"VERIFIED $confirmed/$total formulas in ${elapsedMs}ms (mode=$mode)")
    else
      val preview = mismatches.take(10).mkString(",")
      val more = if mismatches.size > 10 then s",... (${mismatches.size - 10} more)" else ""
      println(s"MISMATCH $confirmed/$total confirmed; ${mismatches.size} not entailed: $preview$more (${elapsedMs}ms, mode=$mode)")

  /** Sanity-check one output bit: run the normaliser, then test OL entailment
   *  in both directions with and without Tseitin axioms. */
  def runCheckEquiv(algoStr: String, aigPath: String, bit: Int): Unit =
    val algo: Formula => Formula = algoStr match
      case "ol"         => (new OLAlgorithm).reducedForm
      case "ocbsl"      => (new OcbslAlgorithm).reducedForm
      case "structural" => (new OLAlgorithmStructural).reducedForm
      case _            => println(s"Unknown algo: $algoStr"); sys.exit(1)
    val (formulas, nInputs) = AigerParser.getAigerFormulasWithInfo(aigPath)
    val f = formulas(bit)
    val g = algo(f)
    println(s"phi1 size: ${f.circuitSize}  phi2 size: ${g.circuitSize}")
    println(s"same object: ${f eq g}")
    println(s"phi1 <= phi1 (self):        ${EntailmentAlgorithm.isEntailed(f, f, Set.empty)}")
    println(s"phi1 <= phi2 (no axioms):   ${EntailmentAlgorithm.isEntailed(f, g, Set.empty)}")
    println(s"phi2 <= phi1 (no axioms):   ${EntailmentAlgorithm.isEntailed(g, f, Set.empty)}")
    val startVar = 2 * (nInputs + 1)
    val (roots1, axioms1, nextVar) = AigerParser.tseitinAxioms(Seq(f), startVar)
    val (roots2, axioms2, _)       = AigerParser.tseitinAxioms(Seq(g), nextVar)
    val axioms = axioms1 ++ axioms2
    println(s"phi1 <= phi2 (Tseitin OL):  ${EntailmentAlgorithm.isEntailed(roots1(0), roots2(0), axioms)}")
    println(s"phi2 <= phi1 (Tseitin OL):  ${EntailmentAlgorithm.isEntailed(roots2(0), roots1(0), axioms)}")

  /** Generate `number` random formulas of up to `size` nodes and `vars` variables,
   *  normalise with OL (and optionally OCBSL), write results to `<folder>/results/random.txt`.
   *  Pass `-check` to verify equivalence, `-nocbsl` / `-nol` to skip an algorithm. */
  def runRandomBench(number: Int, size: Int, vars: Int, folder: String,
                     check: Boolean = false, ocbsl: Boolean = true, ol: Boolean = true): Unit =
    Benchmark.saveRandomBenchmark(number, size, vars, check, folder, ocbsl, ol)

  /** Normalise all output bits of an AIGER circuit and print stats. */
  def runNormalize(algoStr: String, path: String): Unit =
    val algo = algoStr match
      case "ocbsl"      => Algo.OCBSL
      case "ol"         => Algo.OL
      case "structural" => Algo.STRUCTURAL
      case _            => println(s"Unknown algo: $algoStr"); sys.exit(1)
    val formulas = AigerParser.getAigerFormulas(path)
    val (_, nInputs) = AigerParser.getAigerFormulasWithInfo(path)
    val t0 = System.nanoTime()
    val value = benchmark(algo, formulas)
    val elapsedMs = (System.nanoTime() - t0) / 1_000_000
    println(s"circuit:     $path")
    println(s"algo:        $algo")
    println(s"inputs:      $nInputs")
    println(s"outputs:     ${formulas.length}")
    println(s"size_before: ${value.original}")
    println(s"size_after:  ${value.reduced}")
    println(f"ratio:       ${value.ratio}%.4f")
    println(s"time_ms:     $elapsedMs")
    println(s"time_s:      ${elapsedMs / 1000.0}")
    println(s"RESULT ${value.original} ${value.reduced} $elapsedMs")

  // ── benchmark helpers ──────────────────────────────────────────────────────

  def benchmark(algo: Algo, formulas: List[Formula]): Improvement = algo match {
    case Algo.OCBSL      => benchmarkOcbsl(formulas)
    case Algo.OL         => benchmarkOl(formulas)
    case Algo.STRUCTURAL => benchmarkStructural(formulas)
  }

  def benchmarkOcbsl(formulas: List[Formula]): Improvement = {
    val algo = new OcbslAlgorithm
    val total: (BigInt, BigInt) = (circuitSize(formulas), circuitSize(formulas map algo.reducedForm))
    Improvement(total._1, total._2, (BigDecimal(total._2) / BigDecimal(total._1)).toDouble)
  }

  def benchmarkOl(formulas: List[Formula]): Improvement = {
    val algo = new OLAlgorithm
    val total: (BigInt, BigInt) = (circuitSize(formulas), circuitSize(formulas map algo.reducedForm))
    Improvement(total._1, total._2, (BigDecimal(total._2) / BigDecimal(total._1)).toDouble)
  }

  def benchmarkStructural(formulas: List[Formula]): Improvement = {
    val algo = new OLAlgorithmStructural
    val total: (BigInt, BigInt) = (circuitSize(formulas), circuitSize(formulas map algo.reducedForm))
    Improvement(total._1, total._2, (BigDecimal(total._2) / BigDecimal(total._1)).toDouble)
  }

  // ── private helpers ────────────────────────────────────────────────────────

  /** Build the OL entailment problem (lhs ≤ rhs) for a given mode.
   *  Used by the non-Tseitin entailment paths; Tseitin paths build their
   *  own lhs/rhs from renamed roots. */
  private def problemForMode(mode: String, f: Formula): (Formula, Formula) = mode match {
    case "validity"    => (Literal(true), f)
    case "reflexivity" => (f, f)
    case other         => sys.error(s"Unknown verify mode: $other")
  }

}
