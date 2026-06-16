package ortholattices

import ortholattices.Helpers.*
import ortholattices.algorithms.Datastructures.*
import ortholattices.algorithms.{EntailmentAlgorithm, OLAlgorithm, OLAlgorithmStructural}

/**
 * Round-trip property test: for any formula `f`, if `g = OLnorm(f)` then both
 * `f ⊢ g` and `g ⊢ f` must be provable in OL **without axioms** by the
 * EntailmentAlgorithm. This must hold by definition of OL normalization
 * (closure under reflexivity + substitution of equivalents).
 *
 * Any failure here indicates a bug in either the EntailmentAlgorithm or the
 * OL normalization (we expect the bug to be in the entailment algorithm).
 */
class NormRoundTripSpec extends munit.FunSuite {

  // Use the OLAlgorithmStructural directly (this is what `OLAlgo.reducedForm`
  // and `OLAlgorithm.reducedForm` both delegate to).
  private val olStructural = new OLAlgorithmStructural
  private val olOld        = new OLAlgorithm

  private def checkRoundTrip(label: String, f: Formula): Unit =
    test(s"$label / structural") {
      val g = olStructural.reducedForm(f)
      val fg = EntailmentAlgorithm.isEntailed(f, g)
      val gf = EntailmentAlgorithm.isEntailed(g, f)
      assert(fg, s"$label: f ⊬ norm(f)\n  f    = $f\n  norm = $g")
      assert(gf, s"$label: norm(f) ⊬ f\n  f    = $f\n  norm = $g")
    }

  // -- Atoms ----------------------------------------------------------------
  private val a = Variable(2)
  private val b = Variable(4)
  private val c = Variable(6)
  private val d = Variable(8)
  private val e = Variable(10)

  // ========================================================================
  // 1. Trivial / constants
  // ========================================================================
  checkRoundTrip("var a", a)
  checkRoundTrip("neg var", Neg(a))
  checkRoundTrip("double neg", Neg(Neg(a)))
  checkRoundTrip("triple neg", Neg(Neg(Neg(a))))
  checkRoundTrip("Top", Literal(true))
  checkRoundTrip("Bot", Literal(false))
  checkRoundTrip("a or true", or(a, Literal(true)))
  checkRoundTrip("a and true", and(a, Literal(true)))
  checkRoundTrip("a or false", or(a, Literal(false)))
  checkRoundTrip("a and false", and(a, Literal(false)))

  // ========================================================================
  // 2. Excluded middle / contradiction (top/bot)
  // ========================================================================
  checkRoundTrip("a or neg a", or(a, Neg(a)))
  checkRoundTrip("a and neg a", and(a, Neg(a)))
  checkRoundTrip("(a or neg a) and b", and(or(a, Neg(a)), b))
  checkRoundTrip("(a and neg a) or b", or(and(a, Neg(a)), b))

  // ========================================================================
  // 3. Idempotence / absorption
  // ========================================================================
  checkRoundTrip("a or a", or(a, a))
  checkRoundTrip("a and a", and(a, a))
  checkRoundTrip("absorption a or (a and b)", or(a, and(a, b)))
  checkRoundTrip("absorption a and (a or b)", and(a, or(a, b)))

  // ========================================================================
  // 4. De Morgan
  // ========================================================================
  checkRoundTrip("neg(a and b)", Neg(and(a, b)))
  checkRoundTrip("neg(a or b)", Neg(or(a, b)))
  checkRoundTrip("neg(a and b and c)", Neg(and(a, b, c)))
  checkRoundTrip("neg(a or b or c)", Neg(or(a, b, c)))

  // ========================================================================
  // 5. AIGER / AIG-like patterns: Neg(And(...)) is the OR-via-NAND encoding
  // ========================================================================
  // Pure NAND
  checkRoundTrip("nand(a,b)",          Neg(and(a, b)))
  checkRoundTrip("nand(neg a, neg b)", Neg(and(Neg(a), Neg(b))))    // = a or b
  checkRoundTrip("AIG OR(a,b)",        Neg(and(Neg(a), Neg(b))))
  checkRoundTrip("AIG OR(a,b,c)",      Neg(and(Neg(a), Neg(b), Neg(c))))
  checkRoundTrip("AIG XOR(a,b)",
    Neg(and(Neg(and(a, Neg(b))), Neg(and(Neg(a), b)))))
  // AIG-style chain: Neg(And(Neg(...), Neg(...))) of depth 3
  checkRoundTrip("AIG chain 1",
    Neg(and(Neg(and(a, b)), Neg(and(c, d)))))
  checkRoundTrip("AIG chain 2",
    Neg(and(Neg(and(a, Neg(and(b, c)))), Neg(and(Neg(d), e)))))

  // ========================================================================
  // 6. Subformula sharing — same Variable referenced multiple times
  // ========================================================================
  checkRoundTrip("a and (a or b)",
    and(a, or(a, b)))
  checkRoundTrip("(a or b) and (a or c)",
    and(or(a, b), or(a, c)))
  checkRoundTrip("(a and b) or (a and c)",
    or(and(a, b), and(a, c)))
  // Shared subformula appearing both positively and negatively
  checkRoundTrip("a and Neg(a or b)",
    and(a, Neg(or(a, b))))
  checkRoundTrip("(a or b) and Neg(a or b)",
    and(or(a, b), Neg(or(a, b))))
  checkRoundTrip("(a and b) or Neg(a and b)",
    or(and(a, b), Neg(and(a, b))))

  // ========================================================================
  // 7. Implications / iff
  // ========================================================================
  checkRoundTrip("a implies b", implies(a, b))
  checkRoundTrip("a iff b", iff(a, b))
  checkRoundTrip("(a iff b) iff (b iff a)", iff(iff(a, b), iff(b, a)))
  checkRoundTrip("(a implies b) implies (a implies b)",
    implies(implies(a, b), implies(a, b)))

  // ========================================================================
  // 8. Larger combinatorial formulas
  // ========================================================================
  checkRoundTrip("distrib a and (b or c)",
    and(a, or(b, c)))
  checkRoundTrip("distrib (a or b) and (c or d)",
    and(or(a, b), or(c, d)))
  checkRoundTrip("nested 1",
    or(and(a, or(b, c)), and(Neg(a), or(b, Neg(c)))))
  checkRoundTrip("nested 2",
    and(or(a, and(b, c)), or(Neg(a), and(Neg(b), Neg(c)))))
  checkRoundTrip("nested 3",
    Neg(or(and(a, b), and(Neg(a), Neg(b)))))   // negated XOR
  checkRoundTrip("Peirce-like",
    implies(implies(implies(a, b), a), a))

  // ========================================================================
  // 9. Many variables / wide gates
  // ========================================================================
  checkRoundTrip("and 5",
    and(a, b, c, d, e))
  checkRoundTrip("or 5",
    or(a, b, c, d, e))
  checkRoundTrip("alt and/or",
    and(or(a, b), or(c, d), or(a, e)))

  // ========================================================================
  // 10. Many tiny adder-like circuits (random seeded)
  // ========================================================================
  // Small random AIG-like formulas built deterministically.
  private def aigRand(seed: Long, depth: Int, vars: List[Formula]): Formula =
    val rng = new scala.util.Random(seed)
    def go(d: Int): Formula =
      if d <= 0 then
        val v = vars(rng.nextInt(vars.length))
        if rng.nextBoolean() then Neg(v) else v
      else
        // Always emit AIG-style: Neg(And(arg1, arg2)) or And(arg1, arg2)
        val left  = go(d - 1)
        val right = go(d - 1)
        val core  = and(left, right)
        if rng.nextBoolean() then Neg(core) else core
    go(depth)

  for seed <- 1 to 20 do
    for depth <- 2 to 5 do
      checkRoundTrip(s"random AIG seed=$seed depth=$depth",
        aigRand(seed.toLong, depth, List(a, b, c, d, e)))

  // ========================================================================
  // 11. Targeted: the adder.aig bit-128 size-5 case
  // ========================================================================
  // Bit 128 of the adder is a sum bit: typically  a XOR b XOR cin
  // encoded as  Neg(And(Neg(And(a, Neg(b))), Neg(And(Neg(a), b))))  XOR cin
  checkRoundTrip("adder-style sum bit",
    Neg(and(
      Neg(and(
        Neg(and(a, Neg(b))),
        Neg(and(Neg(a), b)))),
      Neg(c))))

  // ========================================================================
  // 12. Adversarial polarity flips — same Variable used in both polarities
  // many times, with shared compound subformulas reached through both
  // positive and negative paths.  These specifically stress the
  // polarity-flag bug class in NNF construction.
  // ========================================================================
  checkRoundTrip("polarity stress 1",
    and(or(a, Neg(a)), or(Neg(a), a), and(a, Neg(Neg(a)))))
  checkRoundTrip("polarity stress 2",
    Neg(and(Neg(or(a, b)), or(a, b))))
  checkRoundTrip("shared subformula polarity",
    {
      val s = and(a, b)
      and(s, Neg(s), or(s, Neg(s)))
    })
  checkRoundTrip("deep AIG-OR chain depth 6",
    {
      // OR of 6 vars encoded as nested NAND
      val vars = List(a, b, c, d, e, Variable(12))
      vars.reduceLeft((x, y) => Neg(and(Neg(x), Neg(y))))
    })
  checkRoundTrip("XOR chain length 4 (AIG)",
    {
      def xor(p: Formula, q: Formula): Formula =
        Neg(and(Neg(and(p, Neg(q))), Neg(and(Neg(p), q))))
      xor(xor(a, b), xor(c, d))
    })

  // ========================================================================
  // 13. Larger random AIG circuits — wider reach, more shared atoms
  // ========================================================================
  for seed <- 21 to 60 do
    for depth <- 3 to 6 do
      checkRoundTrip(s"random AIG L seed=$seed depth=$depth",
        aigRand(seed.toLong, depth, List(a, b, c, d, e)))

  // ========================================================================
  // 14. Random circuits with explicit subformula sharing (DAG, not tree)
  // ========================================================================
  private def aigDag(seed: Long, nNodes: Int, vars: List[Formula]): Formula =
    val rng = new scala.util.Random(seed)
    val pool = scala.collection.mutable.ArrayBuffer[Formula]()
    pool ++= vars
    for _ <- 0 until nNodes do
      val l = pool(rng.nextInt(pool.length))
      val r = pool(rng.nextInt(pool.length))
      val core: Formula = and(l, r)
      val node = if rng.nextBoolean() then Neg(core) else core
      pool += node
    pool.last

  for seed <- 1 to 30 do
    for n <- List(4, 8, 12, 18) do
      checkRoundTrip(s"random AIG-DAG seed=$seed n=$n",
        aigDag(seed.toLong, n, List(a, b, c, d, e)))

  // ========================================================================
  // 15. Identity-distinct atoms: rebuild fresh Variable objects with the
  // same id and check that the algorithm still treats them as the same atom.
  // (This is the original AIGER-parser bug class.)
  // ========================================================================
  test("identity-distinct atoms / structural") {
    val freshA = Variable(2)
    val freshB = Variable(4)
    assert(!(freshA eq a), "test setup: fresh Variable should be a different object")
    val f = and(or(freshA, freshB), Neg(freshA))
    val g = olStructural.reducedForm(f)
    assert(EntailmentAlgorithm.isEntailed(f, g), s"f ⊬ norm(f); f=$f g=$g")
    assert(EntailmentAlgorithm.isEntailed(g, f), s"norm(f) ⊬ f; f=$f g=$g")
    // Cross-check with completely distinct Formula trees that contain the
    // ORIGINAL atom objects (a, b) on one side and freshly-built copies on
    // the other.
    val f2 = and(a, or(a, b))
    val g2 = and(freshA, or(freshA, freshB))
    assert(EntailmentAlgorithm.isEntailed(f2, g2), s"identical structure, distinct objects: $f2 ⊬ $g2")
    assert(EntailmentAlgorithm.isEntailed(g2, f2), s"identical structure, distinct objects: $g2 ⊬ $f2")
  }
}
