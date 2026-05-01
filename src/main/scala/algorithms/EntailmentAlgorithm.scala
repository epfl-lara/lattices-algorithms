package ortholattices.algorithms

import Datastructures.*

import scala.collection.mutable

/**
 * Forward worklist entailment algorithm for orthologic with function symbols.
 *
 * Port of oltypes/Subtyping.scala to the lattices-algorithms Formula type.
 * Proves a ≤ b in OL+ by forward-chaining on sequents (φ, ψ) representing φ ∨ ψ = ⊤.
 *
 * Rules:
 *   - Hyp:        a ∨ ¬a = ⊤ for every atom
 *   - Or:         if a ∨ b = ⊤ and a is a child of (a ∨ c) ∈ SF, then (a ∨ c) ∨ b = ⊤
 *   - And:        if a ∨ b = ⊤ and c ∨ b = ⊤ and (a ∧ c) ∈ SF, then (a ∧ c) ∨ b = ⊤
 *   - Cut:        if a ∨ b = ⊤ and a ≤ d (from earlier sequents), then d ∨ b = ⊤
 *   - Replace:    if a ∨ a = ⊤ (i.e. a = ⊤), then φ ∨ a = ⊤ for all φ ∈ SF
 *   - Congruence: propagate subtyping through constructor arguments respecting variance
 *
 * Complexity: O(n³) worst case where n = number of subformulas.
 */
object EntailmentAlgorithm {

  /** Aggregate stats accumulated across all prove() calls for one circuit. */
  object Stats:
    var sfSizeTotal    = 0
    var atomsTotal     = 0
    var seedSeqsTotal  = 0
    var dequeuedTotal  = 0
    var addCallsTotal  = 0
    var addedTotal     = 0
    var provenTotal    = 0
    var callCount      = 0

    def reset(): Unit =
      sfSizeTotal = 0; atomsTotal = 0; seedSeqsTotal = 0
      dequeuedTotal = 0; addCallsTotal = 0; addedTotal = 0
      provenTotal = 0; callCount = 0

    def printToStderr(): Unit =
      System.err.println(
        s"[stats] calls=$callCount  sf_total=$sfSizeTotal  atoms_total=$atomsTotal" +
        s"  seed_seqs=$seedSeqsTotal  dequeued=$dequeuedTotal" +
        s"  add_calls=$addCallsTotal  added=$addedTotal  proven=$provenTotal"
      )

  // =========================================================================
  // NNF representation with cached inverses
  //
  // Negation is pushed to atoms only. Each node caches its inverse
  // (the NNF of its negation), linked bidirectionally.
  // =========================================================================

  private val nnfIdCounter = new java.util.concurrent.atomic.AtomicInteger(0)

  sealed trait NNF:
    private var _inverse: NNF = null

    /** Unique integer ID assigned at construction. Used for O(1) int-keyed maps. */
    val nnfId: Int = nnfIdCounter.getAndIncrement()

    /** Get the inverse (NNF of negation). Computed once and cached. */
    final def inverse: NNF =
      if _inverse == null then
        _inverse = computeInverse()
        _inverse._inverse = this // bidirectional link
      _inverse

    protected def computeInverse(): NNF

    // Use ID-based hashing: O(1) and collision-free (each node has a unique nnfId).
    override def hashCode(): Int = nnfId
    override def equals(other: Any): Boolean = this eq other.asInstanceOf[AnyRef]

  /** Positive variable atom. */
  case class NNFVar(id: Int) extends NNF:
    protected def computeInverse(): NNF = NNFNot(this)

  /** Constructor application atom: F(φ₁, …, φₙ). Arguments are in NNF. */
  case class NNFApp(sym: FunSymbol, args: List[NNF]) extends NNF:
    protected def computeInverse(): NNF = NNFNot(this)

  /** Negated atom (only variables and constructor applications can be negated). */
  case class NNFNot(atom: NNF) extends NNF:
    protected def computeInverse(): NNF = atom

  /** Binary disjunction. */
  case class NNFOr(left: NNF, right: NNF) extends NNF:
    protected def computeInverse(): NNF = NNFAnd(left.inverse, right.inverse)

  /** Binary conjunction. */
  case class NNFAnd(left: NNF, right: NNF) extends NNF:
    protected def computeInverse(): NNF = NNFOr(left.inverse, right.inverse)

  // =========================================================================
  // Formula → NNF conversion
  // =========================================================================

  /** Dummy variable used to encode ⊤ = d ∨ ¬d and ⊥ = d ∧ ¬d. */
  private val dummyVar = NNFVar(Int.MinValue)

  /** Convert a Formula to NNF (positive context). Memoized on Formula.nnfP. */
  def toNNF(f: Formula): NNF =
    f.nnfP match
      case Some(cached) => return cached
      case None => ()
    val r: NNF = f match
      case Variable(id)              => NNFVar(id)
      case Neg(child)                => toNNFNeg(child)
      case Or(children) if children.nonEmpty =>
        children.map(toNNF).reduceRight(NNFOr(_, _))
      case And(children) if children.nonEmpty =>
        children.map(toNNF).reduceRight(NNFAnd(_, _))
      case FunApplication(sym, args) => NNFApp(sym, args.map(toNNF))
      case Literal(true)             => NNFOr(dummyVar, dummyVar.inverse)
      case Literal(false)            => NNFAnd(dummyVar, dummyVar.inverse)
      case Or(_)                     => NNFAnd(dummyVar, dummyVar.inverse)  // empty Or = ⊥
      case And(_)                    => NNFOr(dummyVar, dummyVar.inverse)   // empty And = ⊤
    f.nnfP = Some(r)
    r

  /** Convert ¬f to NNF (push negation inward). Memoized on Formula.nnfN. */
  private def toNNFNeg(f: Formula): NNF =
    f.nnfN match
      case Some(cached) => return cached
      case None => ()
    val r: NNF = f match
      case Variable(id)              => NNFNot(NNFVar(id))
      case Neg(child)                => toNNF(child)                       // ¬¬a = a
      case Or(children) if children.nonEmpty =>
        children.map(toNNFNeg).reduceRight(NNFAnd(_, _))                   // ¬(a∨b) = ¬a ∧ ¬b
      case And(children) if children.nonEmpty =>
        children.map(toNNFNeg).reduceRight(NNFOr(_, _))                    // ¬(a∧b) = ¬a ∨ ¬b
      case FunApplication(sym, args) =>
        NNFNot(NNFApp(sym, args.map(toNNF)))                               // ¬F(a) stays as ¬F(a)
      case Literal(true)             => NNFAnd(dummyVar, dummyVar.inverse) // ¬⊤ = ⊥
      case Literal(false)            => NNFOr(dummyVar, dummyVar.inverse)  // ¬⊥ = ⊤
      case Or(_)                     => NNFOr(dummyVar, dummyVar.inverse)  // ¬⊥ = ⊤
      case And(_)                    => NNFAnd(dummyVar, dummyVar.inverse) // ¬⊤ = ⊥
    f.nnfN = Some(r)
    r

  // =========================================================================
  // Subformula and atom collection
  // =========================================================================

  /** Accumulate all subformulas of t into `acc` (visited-set to avoid re-traversal). */
  private def collectSubformulas(t: NNF, acc: mutable.Set[NNF]): Unit =
    if acc.add(t) then t match
      case NNFOr(l, r)     => collectSubformulas(l, acc); collectSubformulas(r, acc)
      case NNFAnd(l, r)    => collectSubformulas(l, acc); collectSubformulas(r, acc)
      case NNFNot(a)       => collectSubformulas(a, acc)
      case NNFApp(_, args) => args.foreach(collectSubformulas(_, acc))
      case _               => () // NNFVar — leaf

  /** Legacy wrapper used by generateIntermediateTerms. */
  private def subformulasOf(t: NNF): Set[NNF] =
    val acc = mutable.HashSet[NNF]()
    collectSubformulas(t, acc)
    acc.toSet

  private def subformulasOfSequent(s: (NNF, NNF)): Set[NNF] =
    val acc = mutable.HashSet[NNF]()
    collectSubformulas(s._1, acc)
    collectSubformulas(s._2, acc)
    acc.toSet

  /** Accumulate all atoms (NNFVar, NNFApp) into `acc`. */
  private def collectAtoms(t: NNF, acc: mutable.Set[NNF]): Unit = t match
    case v: NNFVar       => acc.add(v); ()
    case c: NNFApp       => acc.add(c); c.args.foreach(collectAtoms(_, acc))
    case NNFNot(a)       => collectAtoms(a, acc)
    case NNFOr(l, r)     => collectAtoms(l, acc); collectAtoms(r, acc)
    case NNFAnd(l, r)    => collectAtoms(l, acc); collectAtoms(r, acc)

  /** Collect all atoms (NNFVar, NNFApp) for Hyp initialisation. */
  private def atomsOf(t: NNF): Set[NNF] = t match
    case v: NNFVar        => Set(v)
    case c: NNFApp        => Set(c) ++ c.args.flatMap(atomsOf)
    case NNFNot(a)        => atomsOf(a)
    case NNFOr(l, r)      => atomsOf(l) ++ atomsOf(r)
    case NNFAnd(l, r)     => atomsOf(l) ++ atomsOf(r)

  // =========================================================================
  // Constructor position indexing (for efficient congruence)
  //
  // For each formula φ appearing at position i in F(…, φ, …), we store
  // (constructor term, position, variance) so that when φ ≤ ψ is proven
  // we can find matching constructors and fire the congruence rule.
  // =========================================================================

  private case class ConPos(term: NNFApp, pos: Int, variance: Variance)

  /** Build index: formula → set of (constructor term, position, variance). */
  private def buildConPosIndex(subformulas: Set[NNF]): Map[NNF, Set[ConPos]] =
    val index = mutable.Map[NNF, mutable.Set[ConPos]]()
    subformulas.foreach {
      case term @ NNFApp(sym, args) =>
        args.zipWithIndex.foreach { case (arg, pos) =>
          val variance = sym.variances(pos)
          index.getOrElseUpdate(arg, mutable.Set.empty) += ConPos(term, pos, variance)
        }
      case _ => ()
    }
    index.map { case (k, v) => k -> v.toSet }.toMap

  /** Build index: (symbol, position, argument) → set of constructor terms. */
  private def buildTermLookup(subformulas: Set[NNF]): Map[(FunSymbol, Int, NNF), Set[NNFApp]] =
    val lookup = mutable.Map[(FunSymbol, Int, NNF), mutable.Set[NNFApp]]()
    subformulas.foreach {
      case term @ NNFApp(sym, args) =>
        args.zipWithIndex.foreach { case (arg, pos) =>
          lookup.getOrElseUpdate((sym, pos, arg), mutable.Set.empty) += term
        }
      case _ => ()
    }
    lookup.map { case (k, v) => k -> v.toSet }.toMap

  // =========================================================================
  // Intermediate term generation
  //
  // For multi-argument constructors, generates all argument combinations so
  // that congruence can fire one position at a time.  E.g. if Pair(a,b)
  // and Pair(c,d) are in SF, also add Pair(a,d) and Pair(c,b).
  // =========================================================================

  private def generateIntermediateTerms(subformulas: mutable.Set[NNF]): Unit =
    val appGroups = subformulas.toSet.collect { case c: NNFApp => c }.groupBy(_.sym)
    for (sym, terms) <- appGroups do
      val arity = sym.arity
      if arity > 1 then
        val argsByPos = Array.fill(arity)(mutable.Set[NNF]())
        for term <- terms do
          term.args.zipWithIndex.foreach { case (arg, pos) => argsByPos(pos) += arg }

        def generateCombinations(pos: Int, acc: List[NNF]): List[List[NNF]] =
          if pos == arity then List(acc.reverse)
          else argsByPos(pos).toList.flatMap(arg => generateCombinations(pos + 1, arg :: acc))

        for args <- generateCombinations(0, Nil) do
          val term = NNFApp(sym, args)
          if !subformulas.contains(term) then
            subformulas += term
            subformulas ++= subformulasOf(term.inverse)

  // =========================================================================
  // Public API
  // =========================================================================

  /**
   * Check whether f1 ≤ f2 in orthologic (with function symbols).
   *
   * Equivalent to [[OLAlgorithm.isOLSmaller]] but using the forward
   * worklist / sequent-based algorithm instead of normalization.
   */
  def isEntailed(f1: Formula, f2: Formula, axioms: Set[(Formula, Formula)] = Set.empty): Boolean =
    val left  = toNNF(f1).inverse   // ¬f1
    val right = toNNF(f2)           // f2
    val axiomSequents = axioms.map { case (sub, sup) =>
      (toNNF(sub).inverse, toNNF(sup))
    }
    prove((left, right), axiomSequents)

  /**
   * Check whether f1 = f2 in orthologic: f1 ≤ f2 and f2 ≤ f1.
   */
  def isEquivalent(f1: Formula, f2: Formula, axioms: Set[(Formula, Formula)] = Set.empty): Boolean =
    isEntailed(f1, f2, axioms) && isEntailed(f2, f1, axioms)

  // =========================================================================
  // Core worklist algorithm
  // =========================================================================

  private def prove(goal: (NNF, NNF), axioms: Set[(NNF, NNF)]): Boolean =
    // --- Collect all subformulas (goal + axioms + inverses) using mutable accumulation ---
    val subformulas = mutable.HashSet[NNF]()
    collectSubformulas(goal._1, subformulas)
    collectSubformulas(goal._2, subformulas)
    axioms.foreach { case (a, b) => collectSubformulas(a, subformulas); collectSubformulas(b, subformulas) }
    // add inverses of everything collected so far
    val snapshot = subformulas.toArray
    snapshot.foreach(f => collectSubformulas(f.inverse, subformulas))

    // --- Generate intermediate constructor terms for transitivity ---
    generateIntermediateTerms(subformulas)

    // --- Build constructor position indices ---
    val frozenSF  = subformulas.toSet
    val conPosIdx = buildConPosIndex(frozenSF)
    val termLookup = buildTermLookup(frozenSF)

    // --- Collect atoms for Hyp initialisation ---
    val atoms = mutable.HashSet[NNF]()
    subformulas.foreach(f => collectAtoms(f, atoms))

    // --- Assign per-call local integer IDs to all NNF nodes (incl. inverses) ---
    // This lets us use a flat BitSet for proven — no boxing, O(1), cache-friendly.
    val localIdMap = mutable.HashMap[NNF, Int]()
    var localIdCtr = 0
    def localId(n: NNF): Int = localIdMap.getOrElseUpdate(n, { val i = localIdCtr; localIdCtr += 1; i })
    subformulas.foreach { f => localId(f); localId(f.inverse) }
    val n = localIdCtr

    // --- Proven set: flat BitSet keyed by (localId(a)*n + localId(b)) —-- no boxing ---
    val proven   = new java.util.BitSet(n * n)
    val worklist = mutable.Stack[(NNF, NNF)]()

    @inline def provenIdx(a: NNF, b: NNF): Int = localId(a) * n + localId(b)

    var localDequeued = 0
    var localAddCalls = 0
    var localAdded    = 0
    var seedSeqs      = 0

    def addSequent(a: NNF, b: NNF): Unit =
      localAddCalls += 1
      val idx = provenIdx(a, b)
      if !proven.get(idx) then
        localAdded += 1
        proven.set(idx)
        proven.set(provenIdx(b, a))
        worklist.push((a, b))

    // --- Index structures ---
    // pCut(x) = {d | x ≤ d proven},  pAnd(b)(ψ) = {φ | φ = a∧ψ and (a,b) proven}
    val pCut  = mutable.Map[NNF, mutable.Set[NNF]]()
    val pAnd  = mutable.Map[NNF, mutable.Map[NNF, mutable.Set[NNF]]]()
    // sfOr(a) = {c ∈ SF | c = (a ∨ …) or c = (… ∨ a)},  sfAnd dually
    val sfOr  = mutable.Map[NNF, mutable.Set[NNF]]()
    val sfAnd = mutable.Map[NNF, mutable.Set[(NNF, NNF)]]()

    // --- Initialise SF_∨ and SF_∧ ---
    subformulas.foreach {
      case phi @ NNFAnd(phi1, phi2) =>
        sfAnd.getOrElseUpdate(phi1, mutable.Set.empty) += ((phi2, phi))
        sfAnd.getOrElseUpdate(phi2, mutable.Set.empty) += ((phi1, phi))
      case phi @ NNFOr(phi1, phi2) =>
        sfOr.getOrElseUpdate(phi1, mutable.Set.empty) += phi
        sfOr.getOrElseUpdate(phi2, mutable.Set.empty) += phi
      case _ => ()
    }

    // --- Seed: axioms + Hyp ---
    axioms.foreach { case (a, b) => addSequent(a, b) }
    atoms.foreach(x => addSequent(x, x.inverse))

    // --- Seed: Top/Bottom ---
    val topFormulas = subformulas.collect {
      case t @ NNFOr(a, b) if b eq a.inverse => t
    }
    val bottomFormulas = subformulas.collect {
      case t @ NNFAnd(a, b) if b eq a.inverse => t
    }
    for top <- topFormulas; phi <- subformulas if !(phi eq top) do
      addSequent(phi.inverse, top)
    for bot <- bottomFormulas; phi <- subformulas if !(phi eq bot) do
      addSequent(bot.inverse, phi)

    // --- Congruence helper -------------------------------------------------
    seedSeqs = worklist.size

    def accumStats(): Unit =
      Stats.sfSizeTotal   += subformulas.size
      Stats.atomsTotal    += atoms.size
      Stats.seedSeqsTotal += seedSeqs
      Stats.dequeuedTotal += localDequeued
      Stats.addCallsTotal += localAddCalls
      Stats.addedTotal    += localAdded
      Stats.provenTotal   += proven.cardinality() / 2
      Stats.callCount     += 1

    // --- Congruence helper -------------------------------------------------
    // A proven sequent (a, b) means a ∨ b = ⊤, so ¬a ≤ b  and  ¬b ≤ a.
    // For covariant  F at position i:  α ≤ β  ⟹  F[…α…] ≤ F[…β…]
    // For contravariant F:             α ≤ β  ⟹  F[…β…] ≤ F[…α…]
    // -----------------------------------------------------------------------
    def applyCongruenceDir(alpha: NNF, beta: NNF): Unit =
      for ConPos(termAlpha, pos, variance) <- conPosIdx.getOrElse(alpha, Set.empty) do
        for termBeta <- termLookup.getOrElse((termAlpha.sym, pos, beta), Set.empty)
            if termAlpha != termBeta do
          val otherMatch = termAlpha.args.zipWithIndex.zip(termBeta.args).forall {
            case ((argA, i), argB) => i == pos || argA == argB
          }
          if otherMatch then
            variance match
              case Variance.Covariant     => addSequent(termAlpha.inverse, termBeta)
              case Variance.Contravariant => addSequent(termBeta.inverse, termAlpha)
              case Variance.Invariant     => () // skip

    def applyCongruence(a: NNF, b: NNF): Unit =
      applyCongruenceDir(a.inverse, b)   // direction: ¬a ≤ b  →  congruence on (¬a, b)
      applyCongruenceDir(b.inverse, a)   // direction: ¬b ≤ a  →  congruence on (¬b, a)

    // --- Main loop ---------------------------------------------------------
    while worklist.nonEmpty do
      localDequeued += 1
      val (a, b) = worklist.pop()

      // Goal check — identity comparison is correct since NNF uses reference equality
      if (a.nnfId == goal._1.nnfId && b.nnfId == goal._2.nnfId) || (a.nnfId == goal._2.nnfId && b.nnfId == goal._1.nnfId) then
        accumStats()
        return true

      // --- Update P_Cut: (a,b) proven ⟹ ¬a ≤ b and ¬b ≤ a ---
      pCut.getOrElseUpdate(a.inverse, mutable.Set.empty) += b
      pCut.getOrElseUpdate(b.inverse, mutable.Set.empty) += a

      // --- Update P_And ---
      sfAnd.getOrElse(a, mutable.Set.empty).foreach { case (psi, conjunction) =>
        pAnd.getOrElseUpdate(b, mutable.Map.empty)
            .getOrElseUpdate(psi, mutable.Set.empty) += conjunction
      }
      sfAnd.getOrElse(b, mutable.Set.empty).foreach { case (psi, conjunction) =>
        pAnd.getOrElseUpdate(a, mutable.Map.empty)
            .getOrElseUpdate(psi, mutable.Set.empty) += conjunction
      }

      // --- Deduce new sequents (φ, b) — iterate each source directly, no set union ---
      sfOr.getOrElse(a, mutable.Set.empty).foreach(phi => addSequent(phi, b))
      pAnd.getOrElse(b, mutable.Map.empty).getOrElse(a, mutable.Set.empty).foreach(phi => addSequent(phi, b))
      pCut.getOrElse(a, mutable.Set.empty).foreach(phi => addSequent(phi, b))

      // --- Deduce new sequents (a, φ) ---
      sfOr.getOrElse(b, mutable.Set.empty).foreach(phi => addSequent(a, phi))
      pAnd.getOrElse(a, mutable.Map.empty).getOrElse(b, mutable.Set.empty).foreach(phi => addSequent(a, phi))
      pCut.getOrElse(b, mutable.Set.empty).foreach(phi => addSequent(a, phi))

      // --- Replace rule: a = ⊤ ⟹ φ ∨ a = ⊤ for all φ ---
      if a == b then
        for phi <- subformulas do addSequent(phi, a)

      // --- Congruence ---
      applyCongruence(a, b)
    end while

    accumStats()
    false // goal not reached
  end prove
}
