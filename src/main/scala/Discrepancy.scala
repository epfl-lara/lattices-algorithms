package ortholattices

import ortholattices.algorithms.*
import ortholattices.algorithms.Datastructures.*
import ortholattices.algorithms.OLAlgorithm.*

/**
 * Tools for finding, minimizing, and tracing circuit-size discrepancies between
 * OLAlgorithmStructural (tree-preserving) and OLAlgorithm (flat).
 *
 * Three entry points:
 *   sbt "runMain ortholattices.Discrepancy find"      – scan seeds for a discrepancy
 *   sbt "runMain ortholattices.Discrepancy minimize"  – minimize a known discrepancy
 *   sbt "runMain ortholattices.Discrepancy trace"     – trace both algorithms on a formula
 */
object Discrepancy {

  // ── Shared helpers ────────────────────────────────────────────────────────

  def hasDiscrepancy(f: Formula): Boolean = {
    val a2Size  = circuitSize(List(new OLAlgorithmStructural().reducedForm(f)))
    val oldSize = circuitSize(List(new OLAlgorithm().reducedForm(f)))
    a2Size != oldSize
  }

  def formulaSize(f: Formula): Int = f match
    case And(ch)             => 1 + ch.map(formulaSize).sum
    case Or(ch)              => 1 + ch.map(formulaSize).sum
    case Neg(c)              => 1 + formulaSize(c)
    case FunApplication(_, args) => 1 + args.map(formulaSize).sum
    case _                   => 1

  def mkAnd(children: List[Formula]): Formula = children match
    case Nil            => Literal(true)
    case single :: Nil  => single
    case _              => And(children)

  def mkOr(children: List[Formula]): Formula = children match
    case Nil            => Literal(false)
    case single :: Nil  => single
    case _              => Or(children)

  /** All single-step reductions of a formula: drop a child or replace by a child. */
  def reductions(f: Formula): List[Formula] = f match
    case And(children) =>
      val dropOne        = children.indices.toList.map(i => mkAnd(children.patch(i, Nil, 1)))
      val replaceByChild = children
      val reduceChildren = children.indices.toList.flatMap { i =>
        reductions(children(i)).map(c2 => And(children.updated(i, c2)))
      }
      dropOne ++ replaceByChild ++ reduceChildren
    case Or(children) =>
      val dropOne        = children.indices.toList.map(i => mkOr(children.patch(i, Nil, 1)))
      val replaceByChild = children
      val reduceChildren = children.indices.toList.flatMap { i =>
        reductions(children(i)).map(c2 => Or(children.updated(i, c2)))
      }
      dropOne ++ replaceByChild ++ reduceChildren
    case Neg(child) =>
      List(child) ++ reductions(child).map(Neg(_))
    case _ => Nil

  def minimize(f: Formula): Formula = {
    var current = f
    var changed = true
    while changed do
      changed = false
      reductions(current).find(c => hasDiscrepancy(negationNormalForm(c))) match
        case Some(smaller) =>
          current = negationNormalForm(smaller)
          changed = true
          println(s"  reduced to (size ~${formulaSize(current)}): $current")
        case None => ()
    current
  }

  // ── Entry points ──────────────────────────────────────────────────────────

  /** Scan seeds to find the first formula with a circuit-size discrepancy. */
  def find(): Unit = {
    for (size <- 5 to 50; seed <- 0 until 500) {
      val formula  = FormulaGenerator.randomFormula(size, 3, Some(seed), doFlatten = false)
      val a2Algo   = new OLAlgorithmStructural
      val oldAlgo  = new OLAlgorithm
      val a2Reduced  = a2Algo .reducedForm(formula)
      val oldReduced = oldAlgo.reducedForm(formula)
      val a2Size   = circuitSize(List(a2Reduced))
      val oldSize  = circuitSize(List(oldReduced))
      if a2Size != oldSize then
        println(s"=== DISCREPANCY at size=$size seed=$seed ===")
        println(s"  input:      $formula")
        println(s"  a2Reduced:  $a2Reduced  (circuitSize=$a2Size)")
        println(s"  oldReduced: $oldReduced  (circuitSize=$oldSize)")
        println()
    }
  }

  /** Minimize a known discrepancy formula down to its simplest form. */
  def minimize(): Unit = {
    // Edit the line below to point at the formula of interest.
    val formula = FormulaGenerator.randomFormula(41, 3, Some(21), doFlatten = false)
    println(s"Starting formula (size ~${formulaSize(formula)}):")
    println(s"  $formula")

    val a2Algo  = new OLAlgorithmStructural
    val oldAlgo = new OLAlgorithm
    val a2r  = a2Algo .reducedForm(formula)
    val oldr = oldAlgo.reducedForm(formula)
    println(s"  a2  circuitSize=${circuitSize(List(a2r))}: $a2r")
    println(s"  old circuitSize=${circuitSize(List(oldr))}: $oldr")
    println()
    println("Minimizing...")

    val minimal = minimize(formula)
    println()
    println(s"=== MINIMAL FORMULA (size ~${formulaSize(minimal)}) ===")
    println(s"  $minimal")

    val a2Algo2  = new OLAlgorithmStructural
    val oldAlgo2 = new OLAlgorithm
    val a2r2  = a2Algo2 .reducedForm(minimal)
    val oldr2 = oldAlgo2.reducedForm(minimal)
    println(s"  a2  reduced: $a2r2  (circuitSize=${circuitSize(List(a2r2))})")
    println(s"  old reduced: $oldr2  (circuitSize=${circuitSize(List(oldr2))})")
  }

  /** Trace both algorithms step-by-step on a specific formula. */
  def trace(): Unit = {
    // Set f to the formula you want to trace.
    val f: Formula = ???
    println(s"Input: $f")
    println()

    println("=== ALGORITHM 2 ===")
    val a2Algo = new OLAlgorithmStructural
    val pA2    = a2Algo.algo2Polarize(f)
    println(s"Polarized:   $pA2")
    val nfA2   = a2Algo.nPnormalForm(pA2)
    println(s"Normal form: $nfA2")
    val rA2    = toFormula(nfA2)
    println(s"Reduced:     $rA2  circuitSize=${circuitSize(List(rA2))}")
    println()

    println("=== OLD ALGORITHM ===")
    val oldAlgo = new OLAlgorithm
    val pOld    = oldAlgo.oldPolarize(f)
    println(s"Polarized:   $pOld")
    val nfOld   = oldAlgo.nPnormalForm(pOld)
    println(s"Normal form: $nfOld")
    val rOld    = toFormula(nfOld)
    println(s"Reduced:     $rOld  circuitSize=${circuitSize(List(rOld))}")
    println()

    println("=== EQUIVALENCE CHECK ===")
    println(s"a2  ≡ old:   ${OLAlgorithm.isSame(rA2, rOld)}")
    println(s"a2  ≡ input: ${OLAlgorithm.isSame(rA2, f)}")
    println(s"old ≡ input: ${OLAlgorithm.isSame(rOld, f)}")
  }

  def main(args: Array[String]): Unit =
    args.headOption.getOrElse("find") match
      case "find"     => find()
      case "minimize" => minimize()
      case "trace"    => trace()
      case cmd        => println(s"Unknown command '$cmd'. Use: find | minimize | trace")
}
