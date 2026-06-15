package ortholattices.algorithms

import Datastructures.*

import com.zaxxer.sparsebits.SparseBitSet
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

    // Per-node cache of proven sequent pairs.
    // Bit `other.nnfId` is set iff the sequent (this, other) was proven in a previous
    // prove() call with the same axiom context (identified by contextKey reference).
    // A key mismatch triggers a lazy clear — no global node iteration needed.
    // _provenWithList keeps NNF references so seeding in the next call is O(|cached|)
    // rather than O(n²): we only iterate actually-proven partners, not all n² pairs.
    private val _provenWith: SparseBitSet = new SparseBitSet()
    private var _provenWithKey: AnyRef = null
    private var _provenWithList: java.util.ArrayList[NNF] = null

    /** True iff (this, other) was cached for axiom context `key`. */
    final def isProvenWith(other: NNF, key: AnyRef): Boolean =
      (_provenWithKey eq key) && _provenWith.get(other.nnfId)

    /** Record (this, other) as proven; lazily clears stale entries on context change. */
    final def setProvenWith(other: NNF, key: AnyRef): Unit =
      if !(_provenWithKey eq key) then
        _provenWith.clear()
        if _provenWithList != null then _provenWithList.clear()
        _provenWithKey = key
      if !_provenWith.get(other.nnfId) then   // avoid duplicate list entries
        _provenWith.set(other.nnfId)
        if _provenWithList == null then _provenWithList = new java.util.ArrayList[NNF]()
        _provenWithList.add(other)

    /** Iterate proven-with partners for context `key` that are also in `sf`; call `f` on each. */
    final def foreachProvenWith(key: AnyRef, sf: mutable.HashSet[NNF], f: NNF => Unit): Unit =
      if (_provenWithKey eq key) && _provenWithList != null then
        val it = _provenWithList.iterator()
        while it.hasNext do
          val b = it.next()
          if sf.contains(b) then f(b)

  /** Positive variable atom. */
  case class NNFVar(id: Int) extends NNF:
    protected def computeInverse(): NNF = NNFNot(this)

  /** Constructor application atom: F(φ₁, …, φₙ). Arguments are in NNF. */
  case class NNFApp(sym: FunSymbol, args: List[NNF]) extends NNF:
    protected def computeInverse(): NNF = NNFNot(this)

  // -------------------------------------------------------------------------
  // Hash-cons caches for ATOMS ONLY (NNFVar, NNFApp).
  //
  // The algorithm uses object identity for NNF equality (hashCode = nnfId,
  // equals = `this eq that`) for O(1) hash/bitset operations.
  //
  // Atoms MUST be hash-consed for completeness: the same Variable(id) (or
  // FunApplication) appearing in two different Formula objects would otherwise
  // produce two distinct NNFVar/NNFApp nodes, which the algorithm treats as
  // unrelated atoms — making it unable to relate `f` to `OLnorm(f)` even
  // though they are OL-equivalent by definition.
  //
  // Compound nodes (NNFOr, NNFAnd, NNFNot) are intentionally NOT hash-consed:
  //  - The And/Or rules of the worklist algorithm derive entailments between
  //    structurally identical compounds via their shared atomic children, so
  //    correctness/completeness does not require compound identity.
  //  - Adding a ConcurrentHashMap lookup + Tuple2 allocation per construction
  //    measurably slows the algorithm on large SF (heavy random-circuit
  //    sweeps regress past the test timeout).
  //  - A persistent global cache for compounds would also leak memory across
  //    long-running benchmarks (unbounded SF growth).
  //
  // Conclusion: identity is used as an EFFICIENCY mechanism (fast hashing,
  // cheap proven-set membership) but completeness only relies on identity
  // for ATOMS, which the structural rules cannot otherwise unify.
  // -------------------------------------------------------------------------
  private val varCache = new java.util.concurrent.ConcurrentHashMap[Int, NNFVar]()
  private val appCache = new java.util.concurrent.ConcurrentHashMap[(FunSymbol, List[NNF]), NNFApp]()

  private def mkVar(id: Int): NNFVar =
    varCache.computeIfAbsent(id, k => NNFVar(k))

  private def mkApp(sym: FunSymbol, args: List[NNF]): NNFApp =
    appCache.computeIfAbsent((sym, args), k => NNFApp(k._1, k._2))

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
  private val dummyVar = mkVar(Int.MinValue)

  /** Convert a Formula to NNF (positive context). Memoized on Formula.nnfP. */
  def toNNF(f: Formula): NNF =
    f.nnfP match
      case Some(cached) => return cached
      case None => ()
    val r: NNF = f match
      case Variable(id)              => mkVar(id)
      case Neg(child)                => toNNFNeg(child)
      case Or(children) if children.nonEmpty =>
        children.map(toNNF).reduceRight(NNFOr(_, _))
      case And(children) if children.nonEmpty =>
        children.map(toNNF).reduceRight(NNFAnd(_, _))
      case FunApplication(sym, args) => mkApp(sym, args.map(toNNF))
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
      case Variable(id)              => toNNF(f).inverse                   // reuse memoized NNFVar + its cached inverse
      case Neg(child)                => toNNF(child)                       // ¬¬a = a
      case Or(children) if children.nonEmpty =>
        children.map(toNNFNeg).reduceRight(NNFAnd(_, _))                   // ¬(a∨b) = ¬a ∧ ¬b
      case And(children) if children.nonEmpty =>
        children.map(toNNFNeg).reduceRight(NNFOr(_, _))                    // ¬(a∧b) = ¬a ∨ ¬b
      case FunApplication(sym, args) => toNNF(f).inverse                   // reuse memoized NNFApp + cached inverse
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
          val cp = ConPos(term, pos, variance)
          index.get(arg) match
            case Some(s) => s += cp
            case None    => index(arg) = mutable.Set(cp)
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
          val key = (sym, pos, arg)
          lookup.get(key) match
            case Some(s) => s += term
            case None    => lookup(key) = mutable.Set(term)
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
          val term = mkApp(sym, args)
          if !subformulas.contains(term) then
            subformulas += term
            subformulas ++= subformulasOf(term.inverse)

  // =========================================================================
  // Public API
  // =========================================================================

  /** Stats returned by [[entailWithStats]]. */
  case class ProveStats(proved: Boolean, nProven: Long, nFormulas: Int)

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
    prove((left, right), axiomSequents, contextKey = axioms, computeAll = false).proved

  /**
   * Check whether f1 = f2 in orthologic: f1 ≤ f2 and f2 ≤ f1.
   */
  def isEquivalent(f1: Formula, f2: Formula, axioms: Set[(Formula, Formula)] = Set.empty): Boolean =
    isEntailed(f1, f2, axioms) && isEntailed(f2, f1, axioms)

  /**
   * Like [[isEntailed]] but always drains the full worklist (no early exit),
   * and additionally returns the number of unique sequents proven and the
   * size of the formula space.  Use this for consistency comparisons between
   * the Scala and C implementations.
   */
  def entailWithStats(f1: Formula, f2: Formula, axioms: Set[(Formula, Formula)] = Set.empty, computeAll: Boolean = true): ProveStats =
    val left  = toNNF(f1).inverse
    val right = toNNF(f2)
    val axiomSequents = axioms.map { case (sub, sup) =>
      (toNNF(sub).inverse, toNNF(sup))
    }
    prove((left, right), axiomSequents, contextKey = axioms, computeAll = computeAll)

  // =========================================================================
  // Core worklist algorithm
  // =========================================================================

  private def prove(goal: (NNF, NNF), axioms: Set[(NNF, NNF)], contextKey: AnyRef, computeAll: Boolean = false): ProveStats =
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
    def localId(n: NNF): Int =
      localIdMap.get(n) match
        case Some(i) => i
        case None    => val i = localIdCtr; localIdCtr += 1; localIdMap(n) = i; i
    subformulas.foreach { f => localId(f); localId(f.inverse) }
    val n = localIdCtr

    // --- Proven set: flat BitSet keyed by (localId(a)*n + localId(b)) —-- no boxing ---
    // Fall back to HashSet[Long] when n² would overflow Int.
    val provenSizeLong = n.toLong * n
    val useBitSet = provenSizeLong <= Int.MaxValue
    val proven    = if useBitSet then new java.util.BitSet(provenSizeLong.toInt) else null
    val provenHS  = if useBitSet then null else new java.util.HashSet[Long]()
    val worklist = mutable.Stack[(NNF, NNF)]()
    var nPushed  = 0L

    @inline def provenIdxL(a: NNF, b: NNF): Long = localId(a).toLong * n + localId(b)
    @inline def provenIdx(a: NNF, b: NNF): Int    = localId(a) * n + localId(b)

    @inline def isProven(a: NNF, b: NNF): Boolean =
      if useBitSet then proven.get(provenIdx(a, b))
      else provenHS.contains(provenIdxL(a, b))

    @inline def setProven(a: NNF, b: NNF): Unit =
      if useBitSet then proven.set(provenIdx(a, b))
      else provenHS.add(provenIdxL(a, b))

    def addSequent(a: NNF, b: NNF): Unit =
      if !isProven(a, b) then
        setProven(a, b)
        setProven(b, a)
        a.setProvenWith(b, contextKey)
        b.setProvenWith(a, contextKey)
        nPushed += 1
        worklist.push((a, b))

    // --- Pre-seed worklist from per-NNF proven cache (mirrors normalization algorithm) ---
    // Sequents proven in earlier calls on shared NNF nodes (same axiom context/generation)
    // are re-injected so the worklist converges without re-deriving them.  The seeding is
    // O(Σ_a |provenWith(a) ∩ subformulas|) — only actual proven pairs, not all n² pairs.
    // Skipped in computeAll mode to keep nProven / nFormulas stats reproducible.
    if !computeAll then
      subformulas.foreach { a => a.foreachProvenWith(contextKey, subformulas, b => addSequent(a, b)) }
      // Fast path: if the goal pair was already cached and re-seeded above, we're done —
      // no need to process any worklist entries.  This makes the common case (outputs that
      // share most subformulas with earlier outputs) essentially O(n) per call.
      if isProven(goal._1, goal._2) || isProven(goal._2, goal._1) then
        return ProveStats(proved = true, nProven = nPushed, nFormulas = n)

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
        val e1 = (phi2, phi); sfAnd.get(phi1) match { case Some(s) => s += e1; case None => sfAnd(phi1) = mutable.Set(e1) }
        val e2 = (phi1, phi); sfAnd.get(phi2) match { case Some(s) => s += e2; case None => sfAnd(phi2) = mutable.Set(e2) }
      case phi @ NNFOr(phi1, phi2) =>
        sfOr.get(phi1) match { case Some(s) => s += phi; case None => sfOr(phi1) = mutable.Set(phi) }
        sfOr.get(phi2) match { case Some(s) => s += phi; case None => sfOr(phi2) = mutable.Set(phi) }
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
      val (a, b) = worklist.pop()

      // Goal check — skip when computeAll, otherwise return immediately
      if !computeAll &&
         ((a.nnfId == goal._1.nnfId && b.nnfId == goal._2.nnfId) ||
          (a.nnfId == goal._2.nnfId && b.nnfId == goal._1.nnfId)) then
        return ProveStats(proved = true, nProven = nPushed, nFormulas = n)

      // --- Update P_Cut: (a,b) proven ⟹ ¬a ≤ b and ¬b ≤ a ---
      val ai = a.inverse
      pCut.get(ai) match { case Some(s) => s += b; case None => pCut(ai) = mutable.Set(b) }
      val bi = b.inverse
      pCut.get(bi) match { case Some(s) => s += a; case None => pCut(bi) = mutable.Set(a) }

      // --- Update P_And ---
      sfAnd.getOrElse(a, mutable.Set.empty).foreach { case (psi, conjunction) =>
        pAnd.get(b) match
          case Some(inner) =>
            inner.get(psi) match { case Some(s) => s += conjunction; case None => inner(psi) = mutable.Set(conjunction) }
          case None =>
            pAnd(b) = mutable.Map(psi -> mutable.Set(conjunction))
      }
      sfAnd.getOrElse(b, mutable.Set.empty).foreach { case (psi, conjunction) =>
        pAnd.get(a) match
          case Some(inner) =>
            inner.get(psi) match { case Some(s) => s += conjunction; case None => inner(psi) = mutable.Set(conjunction) }
          case None =>
            pAnd(a) = mutable.Map(psi -> mutable.Set(conjunction))
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

    // Worklist drained — check whether goal was proven
    val goalProved =
      proven.get(provenIdx(goal._1, goal._2)) ||
      proven.get(provenIdx(goal._2, goal._1))
    ProveStats(proved = goalProved, nProven = nPushed, nFormulas = n)
  end prove
}
