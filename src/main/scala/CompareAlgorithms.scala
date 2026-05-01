package ortholattices

import ortholattices.algorithms.*
import ortholattices.algorithms.Datastructures.*
import ortholattices.algorithms.OLAlgorithm.*

/**
 * Compare two OL normalization algorithms:
 *   - OLAlgorithmStructural (tree-preserving)
 *   - OLAlgorithm (flat/flattening baseline)
 *
 * For each random formula/circuit, measures:
 *   - Time to compute the reduced form (ms)
 *   - Circuit size (DAG) of the output via the existing `circuitSize` function
 *
 * Usage:
 *   sbt "runMain ortholattices.CompareAlgorithms [minSize] [maxSize] [step] [numVars] [samplesPerSize]"
 *   Defaults: minSize=10, maxSize=200, step=10, numVars=5, samplesPerSize=20
 *
 * Runs two benchmarks:
 *   Section A – random trees (FormulaGenerator)
 *   Section B – random circuits with structure sharing (CircuitGenerator)
 */
object CompareAlgorithms {

  private def header(): Unit = {
    println(f"${"size"}%6s | ${"a2Time"}%8s ${"oldTime"}%8s | ${"inCirc"}%7s ${"a2Out"}%7s ${"oldOut"}%7s | ${"a2LEQ"}%8s ${"oldLEQ"}%8s | ${"a2/old"}%7s ${"a2 P3%"}%6s")
    println("-" * 100)
  }

  private def runBenchmark(size: Int, samplesPerSize: Int, formulaForSample: (Int, Int) => Formula): Unit = {
    var totalA2Time     = 0L
    var totalOldTime    = 0L
    var totalInCircuit  = BigInt(0)
    var totalA2Out      = BigInt(0)
    var totalOldOut     = BigInt(0)
    var totalA2Leq      = 0L
    var totalOldLeq     = 0L
    var totalA2Phase3   = 0L

    for (sample <- 0 until samplesPerSize) {
      val formula = formulaForSample(size, sample)
      totalInCircuit += formula.circuitSize

      // --- Algorithm2 (tree-preserving) ------------------------------------
      val a2Algo = new OLAlgorithmStructural
      val t0 = System.nanoTime()
      val a2Reduced = a2Algo.reducedForm(formula)
      val t1 = System.nanoTime()

      // --- Old algorithm (flattening) --------------------------------------
      val oldAlgo = new OLAlgorithm
      val t2 = System.nanoTime()
      val oldReduced = oldAlgo.reducedForm(formula)
      val t3 = System.nanoTime()

      totalA2Time   += (t1 - t0)
      totalOldTime  += (t3 - t2)
      totalA2Out    += circuitSize(List(a2Reduced))
      totalOldOut   += circuitSize(List(oldReduced))
      totalA2Leq    += a2Algo.leqCount
      totalOldLeq   += oldAlgo.leqCount
      totalA2Phase3 += a2Algo.phase3TimeNs
    }

    val n = samplesPerSize
    val avgA2Ms      = totalA2Time.toDouble  / n / 1e6
    val avgOldMs     = totalOldTime.toDouble / n / 1e6
    val avgInCircuit = totalInCircuit.toDouble / n
    val avgA2Out     = totalA2Out.toDouble     / n
    val avgOldOut    = totalOldOut.toDouble    / n
    val avgA2Leq     = totalA2Leq.toDouble  / n
    val avgOldLeq    = totalOldLeq.toDouble / n
    val a2LeqRatio  = if avgOldLeq > 0 then avgA2Leq  / avgOldLeq else Double.NaN
    val a2Phase3Pct = if totalA2Time > 0 then totalA2Phase3.toDouble / totalA2Time * 100.0 else 0.0

    println(f"$size%6d | $avgA2Ms%8.3f $avgOldMs%8.3f | $avgInCircuit%7.1f $avgA2Out%7.1f $avgOldOut%7.1f | $avgA2Leq%8.1f $avgOldLeq%8.1f | $a2LeqRatio%7.3f $a2Phase3Pct%6.1f%%")
  }

  def main(args: Array[String]): Unit = {
    val minSize        = if args.length > 0 then args(0).toInt else 10
    val maxSize        = if args.length > 1 then args(1).toInt else 200
    val step           = if args.length > 2 then args(2).toInt else 10
    val numVars        = if args.length > 3 then args(3).toInt else 5
    val samplesPerSize = if args.length > 4 then args(4).toInt else 20

    // ── Section A: random formula trees ────────────────────────────────────
    println(s"\n=== Section A: Random formula trees (FormulaGenerator, numVars=$numVars) ===")
    header()
    for (size <- minSize to maxSize by step)
      runBenchmark(size, samplesPerSize,
        (sz, sample) => FormulaGenerator.randomFormula(sz, numVars, Some(sz * 1000 + sample), doFlatten = false))

    // ── Section B: random circuits with structure sharing ──────────────────
    println(s"\n=== Section B: Random circuits with structure sharing (CircuitGenerator, numVars=$numVars) ===")
    header()
    for (size <- minSize to maxSize by step)
      runBenchmark(size, samplesPerSize,
        (sz, sample) => CircuitGenerator.randomCircuit(sz, numVars, Some(sz * 1000 + sample)))

    // ── Section C: high-sharing circuits ──────────────────────────────────
    println(s"\n=== Section C: High-sharing circuits (CircuitGenerator.highSharingCircuit, numVars=$numVars) ===")
    header()
    for (size <- minSize to maxSize by step)
      runBenchmark(size, samplesPerSize,
        (sz, sample) => CircuitGenerator.highSharingCircuit(sz, numVars, Some(sz * 1000 + sample)))
  }
}
