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
}
