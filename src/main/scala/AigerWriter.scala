package ortholattices

import ortholattices.algorithms.Datastructures.*
import scala.collection.mutable
import java.nio.file.{Files, Paths}

/**
 * Writes a Formula as an ASCII AIGER (AAG) file.
 *
 * The formula is first converted to NNF (negations pushed to leaves only)
 * before encoding. This is necessary because the C aiger reader applies
 * negative_normal_form with a single-visit flag: if the same gate is
 * referenced in both positive and negative polarity, the gate type is
 * determined by first visit and subsequent visits in the other polarity
 * yield wrong OL semantics. After NNF conversion every internal And/Or
 * gate appears in exactly one polarity (positive), so no conflict arises.
 *
 * De Morgan holds in OL (ortholattice), so NNF is an OL-preserving transform.
 *
 * Input variables must be Variable(2*i) for i = 1..nInputs.
 */
object AigerWriter:

  /** Convert formula to negation normal form (negations at leaves only).
   *  Memoized by object identity for efficiency. */
  private def toNNF(f: Formula): Formula =
    val memo = new java.util.IdentityHashMap[Formula, Formula]()
    def go(f: Formula): Formula =
      val cached = memo.get(f)
      if cached != null then return cached
      val result: Formula = f match
        case _: Variable          => f
        case _: Literal           => f
        case Neg(_: Variable)     => f
        case Neg(Literal(b))      => Literal(!b)
        case Neg(Neg(inner))      => go(inner)
        case Neg(And(cs))         => Or(cs.map(c => go(Neg(c))))
        case Neg(Or(cs))          => And(cs.map(c => go(Neg(c))))
        case And(cs)              => And(cs.map(go))
        case Or(cs)               => Or(cs.map(go))
        case Neg(inner)           => Neg(go(inner))  // shouldn't occur after above cases
        case other                => other
      memo.put(f, result)
      result
    go(f)

  def write(formula: Formula, nInputs: Int, path: String): Unit =
    val f0 = toNNF(formula)
    val andGates = mutable.ArrayBuffer[(Int, Int, Int)]() // (lhs_lit, rhs0_lit, rhs1_lit)
    var nextVar  = nInputs + 1     // next AND gate variable index (each gate uses one)
    // After NNF, every internal node appears in exactly one polarity, so
    // identity-based memoization is safe: no gate is ever used in both polarities.
    val memo = new java.util.IdentityHashMap[Formula, java.lang.Integer]()

    def toLit(f: Formula): Int =
      val cached = memo.get(f)
      if cached != null then return cached.intValue
      val lit: Int = f match
        case Variable(id)   => id          // input literal (id = 2*i, already correct)
        case Literal(true)  => 1           // AIGER constant true
        case Literal(false) => 0           // AIGER constant false
        case Neg(inner)     => toLit(inner) ^ 1  // polarity flip — free in AIG (leaf only after NNF)
        case And(cs)        => andLit(cs.map(toLit))
        case Or(cs)         =>
          // De Morgan: Or(a,b,...) = ¬(¬a ∧ ¬b ∧ ...).
          // After NNF the Or node only appears positively so its De Morgan
          // gate is always referenced with a negative literal — no conflict.
          andLit(cs.map(c => toLit(c) ^ 1)) ^ 1
        case other =>
          throw IllegalArgumentException(s"AigerWriter: unexpected formula node: $other")
      memo.put(f, lit)
      lit

    // Build a binary AND tree from a list of literals, returning the root literal.
    def andLit(lits: Seq[Int]): Int = lits match
      case Nil        => 1   // empty conjunction = true
      case Seq(a)     => a
      case Seq(a, b)  =>
        val v   = nextVar; nextVar += 1
        val lhs = 2 * v
        andGates += ((lhs, a, b))
        lhs
      case _ =>
        val mid  = lits.length / 2
        val l    = andLit(lits.take(mid))
        val r    = andLit(lits.drop(mid))
        andLit(Seq(l, r))

    val outputLit = toLit(f0)
    val A = andGates.length
    val M = if A == 0 then nInputs else nextVar - 1

    val sb = new StringBuilder
    sb.append(s"aag $M $nInputs 0 1 $A\n")
    for i <- 1 to nInputs do sb.append(s"${2 * i}\n")
    sb.append(s"$outputLit\n")
    for (lhs, rhs0, rhs1) <- andGates do sb.append(s"$lhs $rhs0 $rhs1\n")
    Files.writeString(Paths.get(path), sb.toString)
