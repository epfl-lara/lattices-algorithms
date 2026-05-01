package ortholattices.algorithms

import Datastructures.*
import ortholattices.algorithms.OLAlgorithm.*
import com.zaxxer.sparsebits.SparseBitSet

import scala.collection.mutable

/**
 * Snapshot of the tree-preserving OLAlgorithm as of 2026-03-06.
 * Kept as an intermediate reference alongside OLAlgorithm (latest) and
 * OLAlgorithm (flat). Uses its own memoization fields
 * (algo2PolarNormalForm on PolarFormula, algo2PolarFormula on Formula)
 * so it can run independently on the same formula DAG.
 */
class OLAlgorithmStructural extends EquivalenceAndNormalFormAlgorithm {
  import OLAlgorithm.*

  // Own inverse helpers — shadow the shared OLAlgorithm.* ones so algo2's
  // PolarFormula objects never share inverse state with other algorithm variants.
  private def getInversePolar(f: PolarFormula): PolarFormula =
    f.algo2PolarInverse match
      case Some(value) => value
      case None =>
        val second = f match
          case PolarVariable(id, polarity)             => PolarVariable(id, !polarity)
          case PolarAnd(children, polarity)            => PolarAnd(children, !polarity)
          case PolarFunApplication(sym, args, polarity) => PolarFunApplication(sym, args, !polarity)
          case PolarLiteral(b)                          => PolarLiteral(!b)
        f.algo2PolarInverse = Some(second)
        second.algo2PolarInverse = Some(f)
        second

  var leqCount: Long = 0
  var phase3TimeNs: Long = 0

  def latticesLEQ(formula1: NormalPFormula, formula2: NormalPFormula): Boolean =
    leqCount += 1
    if formula1.uniqueKey == formula2.uniqueKey then true
    else formula1.a2LessThanCached(formula2) match
      case Some(value) => value
      case None =>
        val r = (formula1, formula2) match
          case (NPLiteral(b1), NPLiteral(b2)) => !b1 || b2
          case (NPLiteral(b), _) => !b
          case (_, NPLiteral(b)) => b
          case (NPVariable(id1, polarity1), NPVariable(id2, polarity2)) =>
            id1 == id2 && polarity1 == polarity2
          case (NPFunApplication(sym1, a1, p1), NPFunApplication(sym2, a2, p2)) =>
            sym1 == sym2 && p1 == p2 && a1.zip(a2).zip(sym1.variances).forall { case ((s, t), v) =>
              val effectiveV = if p1 then v else v.flip
              effectiveV match
                case Variance.Covariant => latticesLEQ(s, t)
                case Variance.Contravariant => latticesLEQ(t, s)
                case Variance.Invariant => latticesLEQ(s, t) && latticesLEQ(t, s)
            }
          case (_, NPAnd(children, true)) =>
            children.forall(c => latticesLEQ(formula1, c))
          case (NPAnd(children, false), _) =>
            children.forall(c => latticesLEQ(getInverse(c), formula2))
          case (_: NPVariable | _: NPFunApplication, NPAnd(children, false)) =>
            children.exists(c => latticesLEQ(formula1, getInverse(c)))
          case (NPAnd(children, true), _: NPVariable | _: NPFunApplication) =>
            children.exists(c => latticesLEQ(c, formula2))
          case (NPAnd(children1, true), NPAnd(children2, false)) =>
            children1.exists(c => latticesLEQ(c, formula2)) || children2.exists(c => latticesLEQ(formula1, getInverse(c)))
          case _ => false // incompatible atom types (e.g. Variable vs FunApplication)
        formula1.a2SetLessThanCache(formula2, r)
        r

  /**
   * Collect all "leaf" formulas from a same-polarity NPAnd tree by recursive descent.
   * A leaf is any child that is NOT an NPAnd with the same (positive) polarity.
   * Same-polarity NPAnd nodes are transparent containers (associativity of ∧).
   */
  private def collectLeaves(f: NormalPFormula): List[NormalPFormula] = f match
    case NPAnd(ch, true) => ch.flatMap(collectLeaves)
    case _ => List(f)

  /**
   * Prune specific leaf occurrences from a same-polarity NPAnd tree.
   * toRemove maps uniqueKey → remaining removal count (mutated during traversal).
   * remCount(0) tracks total remaining removals for early termination.
   */
  private def pruneTree(f: NormalPFormula, toRemove: mutable.Map[Int, Int], remCount: Array[Int]): Option[NormalPFormula] =
    if remCount(0) == 0 then return Some(f) // early termination: nothing left to remove
    f match
      case n @ NPAnd(ch, true) =>
        var changed = false
        val buf = new mutable.ListBuffer[NormalPFormula]
        ch.foreach { c =>
          pruneTree(c, toRemove, remCount) match
            case Some(c2) =>
              if !(c2 eq c) then changed = true
              buf += c2
            case None =>
              changed = true
        }
        if !changed then Some(n)
        else buf.toList match
          case Nil => None
          case single :: Nil => Some(single)
          case list => Some(NPAnd(list, true))
      case other =>
        toRemove.get(other.uniqueKey) match
          case Some(count) if count > 0 =>
            toRemove.update(other.uniqueKey, count - 1)
            remCount(0) -= 1
            None
          case _ => Some(other)

  /**
   * Combined tree walk: applies absorption replacements AND idempotence removals
   * in a single pass over the original tree structure.
   * - absMap: uniqueKey → absorption result (only entries that changed)
   * - toRemove: uniqueKey → count of leaves to prune (from idempotence)
   * - repCount(0): total leaf occurrences left to replace (early termination)
   * - remCount(0): total leaf occurrences left to remove (early termination)
   * At each absorbed leaf, the replacement is spliced in and pruneTree is
   * applied to it so that toRemove counts are consumed left-to-right.
   */
  private def transformTree(
    f: NormalPFormula,
    absMap: mutable.Map[Int, NormalPFormula],
    toRemove: mutable.Map[Int, Int],
    repCount: Array[Int],
    remCount: Array[Int]
  ): Option[NormalPFormula] =
    if repCount(0) <= 0 && remCount(0) <= 0 then return Some(f)
    f match
      case n @ NPAnd(ch, true) =>
        var changed = false
        val buf = new mutable.ListBuffer[NormalPFormula]
        ch.foreach { c =>
          transformTree(c, absMap, toRemove, repCount, remCount) match
            case Some(c2) =>
              if !(c2 eq c) then changed = true
              buf += c2
            case None =>
              changed = true
        }
        if !changed then Some(n)
        else buf.toList match
          case Nil => None
          case single :: Nil => Some(single)
          case list => Some(NPAnd(list, true))
      case other =>
        // Check absorption replacement first
        val repl = if repCount(0) > 0 then absMap.get(other.uniqueKey) else None
        repl match
          case Some(replacement) =>
            repCount(0) -= 1
            // Splice in absorption result, then prune any idempotence removals from it
            if remCount(0) > 0 then pruneTree(replacement, toRemove, remCount)
            else Some(replacement)
          case None =>
            // Check idempotence removal
            if remCount(0) > 0 then
              toRemove.get(other.uniqueKey) match
                case Some(count) if count > 0 =>
                  toRemove.update(other.uniqueKey, count - 1)
                  remCount(0) -= 1
                  None
                case _ => Some(other)
            else Some(other)

  def simplify(children: List[NormalPFormula], polarity: Boolean): NormalPFormula = {
    val allFlatLeaves = children.flatMap(collectLeaves)
    val nonSimplified = NPAnd(allFlatLeaves, polarity)

    // --- Phase 1: Absorption -----------------------------------------------
    // Process each flat leaf through absorption.  Prepend post-absorption
    // leaves directly to `remaining` (reversed order, matching old algo's
    // treatChild prepend pattern).  Track whether any absorption happened.
    var remaining: List[NormalPFormula] = Nil
    var anyAbsorbed = false

    def applyAbsorption(i: NormalPFormula): NormalPFormula = i match
      case n @ NPAnd(ch, true) =>
        // Tree from absorption replacement — recurse into children
        var changed = false
        val newCh = ch.map { c =>
          val c2 = applyAbsorption(c)
          if !(c2 eq c) then changed = true
          c2
        }
        if !changed then n else NPAnd(newCh, true)
      case NPAnd(ch, false) =>
        if polarity then
          ch.find(c => latticesLEQ(nonSimplified, getInverse(c))) match
            case Some(c) => applyAbsorption(getInverse(c))
            case None =>
              remaining = i :: remaining
              i
        else
          ch.find(f => latticesLEQ(f, nonSimplified)) match
            case Some(value) => applyAbsorption(getInverse(value))
            case None =>
              remaining = i :: remaining
              i
      case leaf =>
        remaining = leaf :: remaining
        leaf

    // Build absMap: uniqueKey → absorption result (only for changed leaves)
    val absMap = mutable.Map[Int, NormalPFormula]()
    var totalAbsorbed = 0
    allFlatLeaves.foreach { leaf =>
      val result = applyAbsorption(leaf)
      if !(result eq leaf) then
        absMap(leaf.uniqueKey) = result
        totalAbsorbed += 1
        anyAbsorbed = true
    }

    // --- Phase 2: Idempotence ----------------------------------------------
    // `remaining` is already in reversed order from prepending.
    // Build toRemove map inline: track leaves removed by idempotence.
    val toRemove = mutable.Map[Int, Int]()
    var totalToRemove = 0
    var accepted: List[NormalPFormula] = Nil
    var acceptedCount = 0
    while remaining.nonEmpty do {
      val current = remaining.head
      remaining = remaining.tail
      if !latticesLEQ(NPAnd(remaining ++ accepted, true), current) then
        accepted = current :: accepted
        acceptedCount += 1
      else
        toRemove(current.uniqueKey) = toRemove.getOrElse(current.uniqueKey, 0) + 1
        totalToRemove += 1
    }

    // --- Phase 2.5: Contradiction check on flat result ---------------------
    if acceptedCount >= 2 then
      val flatResult = NPAnd(accepted, polarity)
      if checkForContradiction(flatResult) then
        return NPLiteral(!polarity)

    // --- Phase 3: Apply absorption + idempotence to original tree ----------
    val t3 = System.nanoTime()
    val resultChildren =
      if acceptedCount == 0 then
        Nil
      else if !anyAbsorbed && totalToRemove == 0 then
        // Nothing changed — return original tree as-is
        children
      else
        // Single-pass tree walk: replace absorbed leaves and prune
        // idempotence-removed leaves from the original `children` tree.
        val repCount = Array(totalAbsorbed)
        val remCount = Array(totalToRemove)
        children.flatMap(c => transformTree(c, absMap, toRemove, repCount, remCount).toList)
    phase3TimeNs += System.nanoTime() - t3

    val r = resultChildren match
      case Nil => NPLiteral(polarity)
      case single :: Nil => if polarity then single else getInverse(single)
      case _ => NPAnd(resultChildren, polarity)
    r
  }

  def checkForContradiction(f: NPAnd): Boolean = {
    f match
      case NPAnd(children, false) =>
        children.exists(cc => latticesLEQ(cc, f))
      case NPAnd(children, true) =>
        val shadowChildren = children map getInverse
        shadowChildren.exists(sc => latticesLEQ(f, sc))
  }

  /**
   * Build an NPAnd(_, true) tree from a PolarAnd(_, true) without calling
   * simplify. PolarAnd(_, true) always produces NPAnd(_, true), which is
   * transparent to walkTree's flattening/absorption. The caller's simplify
   * handles the full tree in one pass. Non-transparent children (PolarAnd
   * with polarity=false, variables, etc.) get full nPnormalForm.
   *
   * IMPORTANT: results are cached so that the same PolarFormula always maps
   * to the same NormalPFormula object, preserving DAG sharing in the output.
   * Two cache levels:
   *   1. algo2PolarNormalForm — set by nPnormalForm (fully simplified)
   *   2. transparentTreeCache — set by buildTransparentTree (unsimplified tree)
   */
  private val transparentTreeCache = mutable.HashMap[Int, NormalPFormula]()

  private def buildTransparentTree(f: PolarFormula): NormalPFormula =
    f.algo2PolarNormalForm match
      case Some(cached) => cached
      case None => transparentTreeCache.getOrElseUpdate(f.uniqueKey, f match
        case PolarAnd(ch, true) => NPAnd(ch.map(buildTransparentTree), true)
        case _ => nPnormalForm(f)
      )

  def nPnormalForm(formula: PolarFormula): NormalPFormula = {
    formula.algo2PolarNormalForm match
      case Some(value) =>
        value
      case None =>
        val r = formula match
          case PolarVariable(id, true) => NPVariable(id, true)
          case PolarVariable(id, false) =>
            getInverse(nPnormalForm(getInversePolar(formula)))
          case PolarFunApplication(sym, args, true) =>
            val newArgs = args.map(nPnormalForm)
            NPFunApplication(sym, newArgs, true)
          case PolarFunApplication(sym, args, false) =>
            getInverse(nPnormalForm(getInversePolar(formula)))
          case PolarAnd(children, polarity) =>
            val newChildren = children.map(buildTransparentTree)
            simplify(newChildren, polarity)
          case PolarLiteral(b) => NPLiteral(b)
        formula.algo2PolarNormalForm = Some(r)
        r
  }

  def checkEquivalence(formula1: PolarFormula, formula2: PolarFormula): Boolean =
    val a = nPnormalForm(formula1)
    val b = nPnormalForm(formula2)
    latticesLEQ(a, b) && latticesLEQ(b, a)

  /** Polarize using algo2's own cache field on Formula. Tree-preserving (no flattening). */
  def algo2Polarize(f: Formula, polarity: Boolean = true): PolarFormula = {
    if polarity & f.algo2PolarFormula.isDefined then return f.algo2PolarFormula.get
    if !polarity & f.algo2PolarFormula.isDefined then return getInversePolar(f.algo2PolarFormula.get)
    val r = f match {
      case Variable(id) => PolarVariable(id, polarity)
      case Neg(child) => algo2Polarize(child, !polarity)
      case Or(children) =>
        PolarAnd(children.map(algo2Polarize(_, false)), !polarity)
      case And(children) =>
        PolarAnd(children.map(algo2Polarize(_, true)), polarity)
      case FunApplication(sym, args) =>
        PolarFunApplication(sym, args.map(algo2Polarize(_, true)), polarity)
      case Literal(b) => PolarLiteral(b == polarity)
    }
    if polarity then f.algo2PolarFormula = Some(r)
    else f.algo2PolarFormula = Some(getInversePolar(r))
    r
  }

  override def isSame(formula1: Formula, formula2: Formula): Boolean =
    checkEquivalence(algo2Polarize(formula1), algo2Polarize(formula2))

  def isOLSmaller(formula1: Formula, formula2: Formula): Boolean =
    latticesLEQ(nPnormalForm(algo2Polarize(formula1)), nPnormalForm(algo2Polarize(formula2)))

  override def reducedForm(formula: Formula): Formula =
    val p = algo2Polarize(formula)
    val nf = nPnormalForm(p)
    val res = toFormula(nf)
    res
}
