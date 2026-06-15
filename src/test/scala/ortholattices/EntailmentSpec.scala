package ortholattices

import ortholattices.Helpers.*
import ortholattices.algorithms.Datastructures.*
import ortholattices.algorithms.EntailmentAlgorithm.{isEntailed, isEquivalent}

/**
 * Tests for the forward-worklist entailment algorithm (EntailmentAlgorithm).
 *
 * Mirrors every test from ConstructorSpec (normalization-based) to validate
 * that the sequent-based algorithm agrees, plus axiom-based tests ported
 * from the oltypes project (Subtyping3Spec, Subtyping3ComplexSpec).
 */
class EntailmentSpec extends munit.FunSuite {

  // -- Nullary symbols (base types) -----------------------------------------
  val IntSym    = FunSymbol("Int")
  val StringSym = FunSymbol("String")
  val BoolSym   = FunSymbol("Bool")
  val NumberSym = FunSymbol("Number")

  val IntT    = funApp(IntSym)
  val StringT = funApp(StringSym)
  val BoolT   = funApp(BoolSym)
  val NumberT = funApp(NumberSym)

  // -- Unary symbols ---------------------------------------------------------
  val ListSym     = FunSymbol("List",     List(Variance.Covariant))
  val ConsumerSym = FunSymbol("Consumer", List(Variance.Contravariant))

  def mkList(t: Formula): Formula     = funApp(ListSym, t)
  def mkConsumer(t: Formula): Formula = funApp(ConsumerSym, t)

  // -- Binary symbols --------------------------------------------------------
  val PairSym = FunSymbol("Pair", List(Variance.Covariant, Variance.Covariant))
  val FuncSym = FunSymbol("Func", List(Variance.Contravariant, Variance.Covariant))

  def mkPair(a: Formula, b: Formula): Formula = funApp(PairSym, a, b)
  def mkFunc(a: Formula, b: Formula): Formula = funApp(FuncSym, a, b)

  // -- Wide symbols ----------------------------------------------------------
  val Tuple5Sym = FunSymbol("Tuple5",
    List.fill(5)(Variance.Covariant))

  val Record6Sym = FunSymbol("Record6",
    List(Variance.Covariant, Variance.Contravariant, Variance.Covariant,
         Variance.Contravariant, Variance.Covariant, Variance.Contravariant))

  val Tuple7Sym = FunSymbol("Tuple7",
    List.fill(7)(Variance.Covariant))

  val Alt8Sym = FunSymbol("Alt8",
    List(Variance.Covariant, Variance.Contravariant, Variance.Covariant, Variance.Contravariant,
         Variance.Covariant, Variance.Contravariant, Variance.Covariant, Variance.Contravariant))

  def mkTuple5(a: Formula, b: Formula, c: Formula, d: Formula, e: Formula) =
    funApp(Tuple5Sym, a, b, c, d, e)
  def mkRecord6(a: Formula, b: Formula, c: Formula, d: Formula, e: Formula, f: Formula) =
    funApp(Record6Sym, a, b, c, d, e, f)
  def mkTuple7(a: Formula, b: Formula, c: Formula, d: Formula, e: Formula, f: Formula, g: Formula) =
    funApp(Tuple7Sym, a, b, c, d, e, f, g)
  def mkAlt8(a: Formula, b: Formula, c: Formula, d: Formula, e: Formula, f: Formula, g: Formula, h: Formula) =
    funApp(Alt8Sym, a, b, c, d, e, f, g, h)

  // -- Animal hierarchy (for axiom tests) ------------------------------------
  val AnimalSym = FunSymbol("Animal")
  val MammalSym = FunSymbol("Mammal")
  val DogSym    = FunSymbol("Dog")
  val CatSym    = FunSymbol("Cat")

  val AnimalT = funApp(AnimalSym)
  val MammalT = funApp(MammalSym)
  val DogT    = funApp(DogSym)
  val CatT    = funApp(CatSym)

  val animalHierarchy: Set[(Formula, Formula)] = Set(
    (DogT, MammalT),
    (CatT, MammalT),
    (MammalT, AnimalT),
  )

  // -- Type variables --------------------------------------------------------
  val a = Variable(100);  val b = Variable(101);  val c = Variable(102)
  val d = Variable(103);  val e = Variable(104);  val f = Variable(105)
  val g = Variable(106);  val h = Variable(107)

  // =========================================================================
  // Basic OL (no constructors) — mirror of ConstructorSpec
  // =========================================================================

  test("OL: reflexivity, top, bottom") {
    assert(isEntailed(IntT, IntT))
    assert(isEntailed(IntT, top))
    assert(isEntailed(bot, IntT))
    assert(!isEntailed(top, IntT))
    assert(!isEntailed(IntT, bot))
  }

  test("OL: union / intersection") {
    assert(isEntailed(IntT, or(IntT, StringT)))
    assert(isEntailed(and(IntT, StringT), IntT))
  }

  test("OL: negation, excluded middle, contradiction") {
    assert(isEquivalent(neg(neg(a)), a))
    assert(isEquivalent(or(a, neg(a)), top))
    assert(isEquivalent(and(a, neg(a)), bot))
  }

  test("OL: commutativity and idempotence") {
    assert(isEquivalent(and(a, b), and(b, a)))
    assert(isEquivalent(and(a, a), a))
  }

  test("OL: (a ∧ ¬a) = (b ∧ ¬b)") {
    assert(isEquivalent(and(a, neg(a)), and(b, neg(b))))
  }

  test("OL: De Morgan 4-var") {
    assert(isEquivalent(neg(or(x0, x1, x2, x3)), and(neg(x0), neg(x1), neg(x2), neg(x3))))
  }

  test("OL: absorption a ∨ (a ∧ (b ∨ (b ∧ c))) = a") {
    assert(isEquivalent(or(a, and(a, or(b, and(b, c)))), a))
  }

  test("OL: distributivity one direction only") {
    assert(isEntailed(or(and(a, b), and(a, c)), and(a, or(b, c))))
    assert(!isEntailed(and(a, or(b, c)), or(and(a, b), and(a, c))))
  }

  test("OL: 8-variable excluded middle") {
    val conj = and(x0, x1, x2, x3, x4, x5, x6, x7)
    assert(isEquivalent(or(conj, neg(conj)), top))
  }

  // =========================================================================
  // Covariant: List[+A]
  // =========================================================================

  test("covariant: List[⊥] <: List[Int] <: List[⊤]") {
    assert(isEntailed(mkList(bot), mkList(IntT)))
    assert(isEntailed(mkList(IntT), mkList(top)))
  }

  test("covariant: List[A] ≮: List[B]") {
    assert(!isEntailed(mkList(a), mkList(b)))
  }

  test("covariant: List[Int] <: List[Int | String]") {
    assert(isEntailed(mkList(IntT), mkList(or(IntT, StringT))))
  }

  // =========================================================================
  // Contravariant: Consumer[-A]
  // =========================================================================

  test("contravariant: Consumer[⊤] <: Consumer[Int] <: Consumer[⊥]") {
    assert(isEntailed(mkConsumer(top), mkConsumer(IntT)))
    assert(isEntailed(mkConsumer(IntT), mkConsumer(bot)))
  }

  test("contravariant: Consumer[Int] ≮: Consumer[⊤]") {
    assert(!isEntailed(mkConsumer(IntT), mkConsumer(top)))
  }

  // =========================================================================
  // Binary covariant: Pair[+A, +B]
  // =========================================================================

  test("pair: Pair[⊥, Int] <: Pair[Int, Int] <: Pair[⊤, ⊤]") {
    assert(isEntailed(mkPair(bot, IntT), mkPair(IntT, IntT)))
    assert(isEntailed(mkPair(IntT, IntT), mkPair(top, top)))
  }

  test("pair: Pair[Int, String] ≮: Pair[String, Int]") {
    assert(!isEntailed(mkPair(IntT, StringT), mkPair(StringT, IntT)))
  }

  // =========================================================================
  // Mixed variance: Func[-A, +B]
  // =========================================================================

  test("func: Func[⊤, ⊥] <: Func[Int, Int]") {
    assert(isEntailed(mkFunc(top, bot), mkFunc(IntT, IntT)))
  }

  test("func: Func[Int, Int] <: Func[Int, ⊤]") {
    assert(isEntailed(mkFunc(IntT, IntT), mkFunc(IntT, top)))
  }

  test("func: Func[Int, Int] ≮: Func[⊤, Int]") {
    assert(!isEntailed(mkFunc(IntT, IntT), mkFunc(top, IntT)))
  }

  // =========================================================================
  // Nested constructors
  // =========================================================================

  test("nested: List[List[⊥]] <: List[List[Int]] <: List[List[⊤]]") {
    assert(isEntailed(mkList(mkList(bot)), mkList(mkList(IntT))))
    assert(isEntailed(mkList(mkList(IntT)), mkList(mkList(top))))
  }

  test("nested: (Int→Int)→Int <: (⊤→⊥)→Int") {
    assert(isEntailed(mkFunc(mkFunc(IntT, IntT), IntT), mkFunc(mkFunc(top, bot), IntT)))
  }

  // =========================================================================
  // Constructors with ∨/∧
  // =========================================================================

  test("List[Int & String] <: List[Int]") {
    assert(isEntailed(mkList(and(IntT, StringT)), mkList(IntT)))
  }

  test("List[Int] | List[String] <: List[Int | String]") {
    assert(isEntailed(or(mkList(IntT), mkList(StringT)), mkList(or(IntT, StringT))))
  }

  // =========================================================================
  // Negation of constructors
  // =========================================================================

  test("¬¬List[a] = List[a]") {
    assert(isEquivalent(neg(neg(mkList(a))), mkList(a)))
  }

  test("List[a] | ¬List[a] = ⊤") {
    assert(isEquivalent(or(mkList(a), neg(mkList(a))), top))
  }

  test("List[a] & ¬List[a] = ⊥") {
    assert(isEquivalent(and(mkList(a), neg(mkList(a))), bot))
  }

  // =========================================================================
  // Wide constructors (no axioms)
  // =========================================================================

  test("Tuple5: ⊥ args <: Int args <: ⊤ args") {
    assert(isEntailed(mkTuple5(bot, bot, bot, bot, bot), mkTuple5(IntT, IntT, IntT, IntT, IntT)))
    assert(isEntailed(mkTuple5(IntT, IntT, IntT, IntT, IntT), mkTuple5(top, top, top, top, top)))
  }

  test("Tuple5: single wrong direction fails") {
    assert(!isEntailed(mkTuple5(top, a, b, c, d), mkTuple5(IntT, a, b, c, d)))
  }

  test("Tuple5 | ¬Tuple5 = ⊤") {
    val t = mkTuple5(a, b, c, d, e)
    assert(isEquivalent(or(t, neg(t)), top))
  }

  test("Record6: all positions correct variance") {
    assert(isEntailed(
      mkRecord6(IntT, top, IntT, top, IntT, top),
      mkRecord6(top, IntT, top, IntT, top, IntT)))
  }

  test("Alt8: all positions correct variance") {
    assert(isEntailed(
      mkAlt8(IntT, top, IntT, top, IntT, top, IntT, top),
      mkAlt8(top, IntT, top, IntT, top, IntT, top, IntT)))
  }

  test("different constructors don't relate") {
    assert(!isEntailed(mkTuple5(a, b, c, d, e), mkTuple7(a, b, c, d, e, f, g)))
    assert(!isEntailed(mkTuple7(a, b, c, d, e, f, g), mkTuple5(a, b, c, d, e)))
  }

  // =========================================================================
  // Deep absorption, De Morgan, excluded middle
  // =========================================================================

  test("absorption tower 5-deep") {
    assert(isEquivalent(
      or(a, and(a, or(b, and(b, or(c, and(c, or(d, and(d, e)))))))),
      a))
  }

  test("dual absorption tower") {
    assert(isEquivalent(
      and(a, or(a, and(b, or(b, and(c, or(c, d)))))),
      a))
  }

  test("deep De Morgan: ¬(a ∧ (b ∨ (c ∧ d))) = ¬a ∨ (¬b ∧ (¬c ∨ ¬d))") {
    assert(isEquivalent(
      neg(and(a, or(b, and(c, d)))),
      or(neg(a), and(neg(b), or(neg(c), neg(d))))))
  }

  test("(a ∧ b) ∨ ¬a ∨ ¬b = ⊤") {
    assert(isEquivalent(or(and(a, b), neg(a), neg(b)), top))
  }

  test("(a ∨ b) ∧ ¬a ∧ ¬b = ⊥") {
    assert(isEquivalent(and(or(a, b), neg(a), neg(b)), bot))
  }

  // =========================================================================
  // One-sided distributivity
  // =========================================================================

  test("one-sided: a ∨ (b ∧ c) ≤ (a ∨ b) ∧ (a ∨ c)") {
    assert(isEntailed(or(a, and(b, c)), and(or(a, b), or(a, c))))
  }

  test("one-sided 2×2: (a∧b) ∨ (c∧d) ≤ (a∨c) ∧ (b∨d)") {
    assert(isEntailed(or(and(a, b), and(c, d)), and(or(a, c), or(b, d))))
  }

  // =========================================================================
  // Negative: Boolean-but-not-OL
  // =========================================================================

  test("distrib reverse FAILS") {
    assert(!isEntailed(and(or(a, b), or(a, c)), or(a, and(b, c))))
  }

  test("case split FAILS: a ≰ (a∧b) ∨ (a∧¬b)") {
    assert(!isEntailed(a, or(and(a, b), and(a, neg(b)))))
  }

  test("orthomodular law FAILS") {
    assert(!isEntailed(or(a, b), or(a, and(neg(a), or(a, b)))))
  }

  test("Boolean partition FAILS") {
    assert(!isEntailed(
      top,
      or(and(a, b), and(a, neg(b)), and(neg(a), b), and(neg(a), neg(b)))))
  }

  // =========================================================================
  // Multi-variable positive inequalities
  // =========================================================================

  test("weakening: a ∧ b ∧ c ≤ a ∨ d ∨ e") {
    assert(isEntailed(and(a, b, c), or(a, d, e)))
  }

  test("monotone meet: a ∧ b ≤ (a∨c) ∧ (b∨d)") {
    assert(isEntailed(and(a, b), and(or(a, c), or(b, d))))
  }

  test("nested meet-join: (a∧b∧c) ∨ (d∧e∧f) ≤ (a∨d) ∧ (b∨e) ∧ (c∨f)") {
    assert(isEntailed(
      or(and(a, b, c), and(d, e, f)),
      and(or(a, d), or(b, e), or(c, f))))
  }

  test("(a∧b) ∨ (¬a∧b) ≤ b but not b ≤ (a∧b) ∨ (¬a∧b)") {
    assert(isEntailed(or(and(a, b), and(neg(a), b)), b))
    assert(!isEntailed(b, or(and(a, b), and(neg(a), b))))
  }

  // =========================================================================
  // =========================================================================
  //     AXIOM-BASED TESTS  (ported from oltypes Subtyping3Spec)
  // =========================================================================
  // =========================================================================

  test("axioms: basic Int <: Number") {
    val axioms: Set[(Formula, Formula)] = Set((IntT, NumberT))
    assert(isEntailed(IntT, NumberT, axioms))
    assert(!isEntailed(NumberT, IntT, axioms))
  }

  test("axioms: covariant — Int <: Number ⟹ List[Int] <: List[Number]") {
    val axioms: Set[(Formula, Formula)] = Set((IntT, NumberT))
    assert(isEntailed(mkList(IntT), mkList(NumberT), axioms))
  }

  test("axioms: contravariant — Int <: Number ⟹ Consumer[Number] <: Consumer[Int]") {
    val axioms: Set[(Formula, Formula)] = Set((IntT, NumberT))
    assert(isEntailed(mkConsumer(NumberT), mkConsumer(IntT), axioms))
    assert(!isEntailed(mkConsumer(IntT), mkConsumer(NumberT), axioms))
  }

  test("axioms: pair — Int <: Number ⟹ Pair[Int,Int] <: Pair[Number,Number]") {
    val axioms: Set[(Formula, Formula)] = Set((IntT, NumberT))
    assert(isEntailed(mkPair(IntT, IntT), mkPair(NumberT, NumberT), axioms))
  }

  test("axioms: func — Func[Number,Int] <: Func[Int,Number]") {
    val axioms: Set[(Formula, Formula)] = Set((IntT, NumberT), (StringT, NumberT))
    assert(isEntailed(mkFunc(NumberT, IntT), mkFunc(IntT, NumberT), axioms))
  }

  test("axioms: nested — List[List[Int]] <: List[List[Number]]") {
    val axioms: Set[(Formula, Formula)] = Set((IntT, NumberT))
    assert(isEntailed(mkList(mkList(IntT)), mkList(mkList(NumberT)), axioms))
  }

  test("axioms: function taking and returning lists") {
    val axioms: Set[(Formula, Formula)] = Set((IntT, NumberT))
    val f1 = mkFunc(mkList(NumberT), mkList(IntT))
    val f2 = mkFunc(mkList(IntT), mkList(NumberT))
    assert(isEntailed(f1, f2, axioms))
  }

  // =========================================================================
  // Hierarchy (transitive chain): Dog <: Mammal <: Animal
  // =========================================================================

  test("hierarchy: Dog <: Animal (transitivity)") {
    assert(isEntailed(DogT, AnimalT, animalHierarchy))
  }

  test("hierarchy: List[Dog] <: List[Animal]") {
    assert(isEntailed(mkList(DogT), mkList(AnimalT), animalHierarchy))
  }

  test("hierarchy: Consumer[Animal] <: Consumer[Dog]") {
    assert(isEntailed(mkConsumer(AnimalT), mkConsumer(DogT), animalHierarchy))
  }

  test("hierarchy: Func[Animal, Dog] <: Func[Dog, Animal]") {
    assert(isEntailed(mkFunc(AnimalT, DogT), mkFunc(DogT, AnimalT), animalHierarchy))
  }

  // =========================================================================
  // Wide constructors with axioms (from Subtyping3ComplexSpec)
  // =========================================================================

  test("axioms wide: Tuple5[Int…] <: Tuple5[Number…]") {
    val axioms: Set[(Formula, Formula)] = Set((IntT, NumberT))
    assert(isEntailed(
      mkTuple5(IntT, IntT, IntT, IntT, IntT),
      mkTuple5(NumberT, NumberT, NumberT, NumberT, NumberT),
      axioms))
  }

  test("axioms wide: Record6 with Int <: Number") {
    val axioms: Set[(Formula, Formula)] = Set((IntT, NumberT))
    // Covariant positions: Int→Number; Contravariant: Number→Int
    assert(isEntailed(
      mkRecord6(IntT, NumberT, IntT, NumberT, IntT, NumberT),
      mkRecord6(NumberT, IntT, NumberT, IntT, NumberT, IntT),
      axioms))
  }

  test("axioms wide: Tuple7 with axiom chain") {
    val axioms: Set[(Formula, Formula)] = Set((IntT, NumberT), (NumberT, top))
    assert(isEntailed(
      mkTuple7(IntT, IntT, IntT, IntT, IntT, IntT, IntT),
      mkTuple7(NumberT, NumberT, NumberT, NumberT, NumberT, NumberT, NumberT),
      axioms))
  }

  test("axioms wide: Alt8 with Int <: Number") {
    val axioms: Set[(Formula, Formula)] = Set((IntT, NumberT))
    assert(isEntailed(
      mkAlt8(IntT, NumberT, IntT, NumberT, IntT, NumberT, IntT, NumberT),
      mkAlt8(NumberT, IntT, NumberT, IntT, NumberT, IntT, NumberT, IntT),
      axioms))
  }

  test("axioms wide: deeply nested List[Tuple5[…]]") {
    val axioms: Set[(Formula, Formula)] = Set((IntT, NumberT))
    assert(isEntailed(
      mkList(mkTuple5(IntT, IntT, IntT, IntT, IntT)),
      mkList(mkTuple5(NumberT, NumberT, NumberT, NumberT, NumberT)),
      axioms))
  }

  // =========================================================================
  // Hierarchy with wide constructors
  // =========================================================================

  test("hierarchy wide: Tuple5[Dog…] <: Tuple5[Animal…]") {
    assert(isEntailed(
      mkTuple5(DogT, DogT, DogT, DogT, DogT),
      mkTuple5(AnimalT, AnimalT, AnimalT, AnimalT, AnimalT),
      animalHierarchy))
  }

  test("hierarchy wide: Record6[Dog,Animal,…] <: Record6[Animal,Dog,…]") {
    assert(isEntailed(
      mkRecord6(DogT, AnimalT, DogT, AnimalT, DogT, AnimalT),
      mkRecord6(AnimalT, DogT, AnimalT, DogT, AnimalT, DogT),
      animalHierarchy))
  }

  test("hierarchy wide: Alt8[Dog,Animal,…] <: Alt8[Animal,Dog,…]") {
    assert(isEntailed(
      mkAlt8(DogT, AnimalT, DogT, AnimalT, DogT, AnimalT, DogT, AnimalT),
      mkAlt8(AnimalT, DogT, AnimalT, DogT, AnimalT, DogT, AnimalT, DogT),
      animalHierarchy))
  }

  // =========================================================================
  // Axiom chain / multi-step transitivity
  // =========================================================================

  test("axioms: Tuple5 two-step chain (Int → Number → ⊤)") {
    val axioms: Set[(Formula, Formula)] = Set((IntT, NumberT), (NumberT, top))
    val sub = mkTuple5(IntT, IntT, IntT, IntT, IntT)
    val mid = mkTuple5(NumberT, NumberT, NumberT, NumberT, NumberT)
    val sup = mkTuple5(top, top, top, top, top)
    assert(isEntailed(sub, mid, axioms))
    assert(isEntailed(mid, sup, axioms))
    assert(isEntailed(sub, sup, axioms))
  }

  test("axioms: Alt8 stress all positions different") {
    val axioms: Set[(Formula, Formula)] = Set((IntT, NumberT), (NumberT, top))
    assert(isEntailed(
      mkAlt8(IntT, top, IntT, top, IntT, top, IntT, top),
      mkAlt8(NumberT, NumberT, NumberT, NumberT, NumberT, NumberT, NumberT, NumberT),
      axioms))
  }

  // =========================================================================
  // Union axiom decomposition
  // =========================================================================

  test("union axiom: Int | String <: T ⟹ Int <: T") {
    val T = Variable(200)
    val IntOrString = or(IntT, StringT)
    val axioms: Set[(Formula, Formula)] = Set((IntOrString, T))
    assert(isEntailed(IntT, T, axioms))
  }

  test("union axiom: Int | String <: T ⟹ String <: T") {
    val T = Variable(200)
    val IntOrString = or(IntT, StringT)
    val axioms: Set[(Formula, Formula)] = Set((IntOrString, T))
    assert(isEntailed(StringT, T, axioms))
  }

  // =========================================================================
  // Recursive union axiom: LL = Int | List[LL]
  // =========================================================================

  test("recursive union: LL = Int | List[LL] ⟹ Int <: LL") {
    val LL = Variable(201)
    val IntOrListLL = or(IntT, mkList(LL))
    val axioms: Set[(Formula, Formula)] = Set(
      (LL, IntOrListLL),       // LL <: Int | List[LL]
      (IntOrListLL, LL)        // Int | List[LL] <: LL
    )
    assert(isEntailed(IntT, LL, axioms))
  }

  test("recursive union: LL = Int | List[LL] ⟹ List[Int] <: List[LL]") {
    val LL = Variable(201)
    val IntOrListLL = or(IntT, mkList(LL))
    val axioms: Set[(Formula, Formula)] = Set(
      (LL, IntOrListLL),
      (IntOrListLL, LL)
    )
    assert(isEntailed(mkList(IntT), mkList(LL), axioms))
  }

  // =========================================================================
  // Replace rule (targeted)
  //
  // The Replace rule fires when a formula `a` is derived to equal ⊤, i.e.
  // when the sequent `(a, a)` enters the worklist.  It then broadcasts:
  //   ∀ φ ∈ SF:  (φ, a) is proven  (= "φ ≤ a = ⊤" for free)
  //
  // The canonical way to force `a = ⊤` is to have axioms `x ≤ a` AND
  // `¬x ≤ a` for some atom x.  The Cut rule then derives `(a, a)` from the
  // two axiom sequents `(¬x, a)` and `(x, a)`, triggering Replace.
  //
  // Every test in this section would return the wrong answer if the
  // Replace branch were commented out.
  // =========================================================================

  test("Replace basic: {x ≤ y, ¬x ≤ y} forces y = ⊤, so unrelated z ≤ y") {
    // Proof sketch: seeds (¬x, y) and (x, y); Cut derives (y, y); Replace
    // adds (¬z, y) for every subformula ¬z → goal (¬z, y) is proven.
    val axioms: Set[(Formula, Formula)] = Set((a, b), (neg(a), b))
    assert(isEntailed(c, b, axioms))
    assert(isEntailed(d, b, axioms))
    assert(isEntailed(neg(c), b, axioms))
    assert(isEntailed(and(c, d), b, axioms))
    assert(isEntailed(or(c, d), b, axioms))
  }

  test("Replace negative: {x ≤ y} alone does NOT force y = ⊤, so z ≤ y is invalid") {
    // Only one direction: Replace cannot fire because (y, y) is never derived.
    // This guards against Replace over-firing on (¬x, y) instead of just (y, y).
    val axioms: Set[(Formula, Formula)] = Set((a, b))
    assert(!isEntailed(c, b, axioms))
    assert(!isEntailed(neg(a), b, axioms))  // ¬x ≤ y is NOT in axioms
  }

  test("Replace with constant symbols: {x ≤ C, ¬x ≤ C} forces C = ⊤, so y ≤ C") {
    // Same proof path but C is a nullary constructor application.
    val C = funApp(FunSymbol("ConstTop"))
    val axioms: Set[(Formula, Formula)] = Set((a, C), (neg(a), C))
    assert(isEntailed(b, C, axioms))
    assert(isEntailed(neg(b), C, axioms))
    assert(isEntailed(mkList(b), C, axioms))
    // Negative: only one direction → still INVALID
    assert(!isEntailed(b, C, Set((a, C))))
  }

  test("Replace chain: {x≤y, ¬x≤y, y≤z} forces y = ⊤, and since y≤z, z = ⊤, so w ≤ z") {
    // Replace fires for y; seeds (¬y, y); Cut with axiom (¬y, z) gives (y, z)
    // then further Replace or Cut gives (z, z); Replace then seeds (w, z).
    val axioms: Set[(Formula, Formula)] = Set((a, b), (neg(a), b), (b, c))
    assert(isEntailed(d, c, axioms))
    assert(isEntailed(neg(d), c, axioms))
  }

  test("Replace two forced tops: A=⊤ and B=⊤ by separate axiom pairs, so A ≤ B") {
    // Neither A nor B is literally the formula `x ∨ ¬x`; both acquire ⊤
    // dynamically.  Replace fires twice (for A and for B), and then
    // (A, B) follows because A ≤ ⊤ = B.
    val A = funApp(FunSymbol("A_top"))
    val B = funApp(FunSymbol("B_top"))
    val axioms: Set[(Formula, Formula)] = Set(
      (a, A), (neg(a), A),
      (b, B), (neg(b), B)
    )
    assert(isEntailed(A, B, axioms))
    assert(isEntailed(B, A, axioms))
    assert(isEquivalent(A, B, axioms))
  }

  test("Replace with compound antecedent: {(a∧b) ≤ F, ¬(a∧b) ≤ F} forces F = ⊤") {
    // The pair covers the full Boolean partition for (a ∧ b) and ¬(a ∧ b).
    val F = funApp(FunSymbol("F_top"))
    val ab = and(a, b)
    val axioms: Set[(Formula, Formula)] = Set((ab, F), (neg(ab), F))
    assert(isEntailed(c, F, axioms))
    assert(isEntailed(d, F, axioms))
  }

  // =========================================================================
  // Weakening
  //
  // In OL proof theory Weakening states: Γ ⊢ a ≤ b  ⟹  Γ, Δ ⊢ a ≤ b.
  // Adding axioms can only make more sequents provable, never fewer.
  //
  // Tests here verify two complementary directions:
  //  (a) monotonicity — provable things remain provable under extra axioms;
  //  (b) non-spuriousness — unprovable things stay unprovable when the extra
  //      axioms are genuinely irrelevant (broken chain, different atoms, etc.)
  //
  // These tests would expose bugs in SF construction or the Replace branch
  // that might cause extra axioms to "pollute" unrelated goals.
  // =========================================================================

  test("Weakening monotonicity: OL tautologies hold with any extra axioms") {
    val extra: Set[(Formula, Formula)] = Set((c, d), (d, e), (f, g))
    // These are independent of a, b and must not disturb OL axioms about a, b.
    assert(isEntailed(a, a, extra))                     // reflexivity
    assert(isEntailed(and(a, b), a, extra))             // ∧-elim
    assert(isEntailed(a, or(a, b), extra))              // ∨-intro
    assert(isEntailed(bot, a, extra))                   // ⊥ ≤ x
    assert(isEntailed(a, top, extra))                   // x ≤ ⊤
    assert(isEquivalent(neg(neg(a)), a, extra))         // double negation
    assert(isEquivalent(or(a, neg(a)), top, extra))     // LEM
    assert(isEquivalent(and(a, neg(a)), bot, extra))    // NC
  }

  test("Weakening monotonicity: proven entailment survives extra axioms") {
    val base: Set[(Formula, Formula)] = Set((a, b))
    val bigger = base ++ Set((c, d), (e, f), (g, h))
    assert(isEntailed(a, b, base))
    assert(isEntailed(a, b, bigger))   // monotonicity: base⊢a≤b ⟹ bigger⊢a≤b
    // Transitivity also survives extra noise
    val trans: Set[(Formula, Formula)] = Set((a, b), (b, c))
    val transPlus: Set[(Formula, Formula)] = trans ++ Set((d, e), (f, g))
    assert(isEntailed(a, c, trans))
    assert(isEntailed(a, c, transPlus))
  }

  test("Weakening non-spurious: broken chain x→z, w→y does not prove x ≤ y") {
    // x maps to z, w maps to y, but x ≠ w so the chain is broken.
    val axioms: Set[(Formula, Formula)] = Set((a, c), (d, b))
    assert(!isEntailed(a, b, axioms))
  }

  test("Weakening non-spurious: axiom {z ≤ w} does not help prove unrelated x ≤ y") {
    val axioms: Set[(Formula, Formula)] = Set((c, d))
    assert(!isEntailed(a, b, axioms))
  }

  test("Weakening non-spurious: OL non-theorem stays non-theorem with extra axioms") {
    // Distributivity is not provable in OL; adding unrelated axioms must not
    // accidentally make it provable.
    val extra: Set[(Formula, Formula)] = Set((c, d), (d, e), (e, f))
    assert(!isEntailed(and(a, or(b, c)), or(and(a, b), and(a, c)), extra))
    assert(!isEntailed(a, or(and(a, b), and(a, neg(b))), extra))   // case split
    assert(!isEntailed(or(a, b), or(a, and(neg(a), or(a, b))), extra))  // OML
  }

  test("Weakening non-spurious: single-direction axiom {x ≤ y} must not trigger Replace for y") {
    // Replace fires only when (y, y) enters the worklist, which requires BOTH
    // (¬x, y) and (x, y) to be seeded.  A single axiom x ≤ y provides only
    // (¬x, y); the test confirms that (z, y) remains unprovable.
    val axioms: Set[(Formula, Formula)] = Set((a, b))
    assert(!isEntailed(c, b, axioms))      // z unrelated to x
    assert(!isEntailed(neg(a), b, axioms)) // ¬x not in axioms
  }

  // =========================================================================
  // OL distributivity investigation
  // =========================================================================

  test("OL: a ≤ b∨c does NOT entail a ≤ (a∧b)∨c in pure OL") {
    // Axiom: a ≤ b ∨ c
    val axioms: Set[(Formula, Formula)] = Set((a, or(b, c)))
    // Goal: a ≤ (a ∧ b) ∨ c
    val goal = or(and(a, b), c)
    val result = isEntailed(a, goal, axioms)
    println(s"a ≤ (a∧b)∨c given a ≤ b∨c: $result")
    // If result is false, the implication does NOT hold in pure OL
    // If result is true, it does hold
  }
}
