package ortholattices

import ortholattices.Helpers.*
import ortholattices.Helpers.OLAlgo.{isSame, isOLSmaller}
import ortholattices.algorithms.Datastructures.*

/**
 * Tests for OL normalization with function symbols (constructors).
 *
 * Covers: covariant, contravariant, mixed-variance, wide (5–8 ary),
 * nested constructors, constructors with ∨/∧, negation of constructors,
 * and basic OL properties (excluded middle, De Morgan, absorption, etc.).
 */
class ConstructorSpec extends munit.FunSuite {

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

  // -- Type variables --------------------------------------------------------
  val a = Variable(100);  val b = Variable(101);  val c = Variable(102)
  val d = Variable(103);  val e = Variable(104);  val f = Variable(105)
  val g = Variable(106);  val h = Variable(107)

  // =========================================================================
  // Basic OL (no constructors)
  // =========================================================================

  test("OL: reflexivity, top, bottom") {
    assert(isOLSmaller(IntT, IntT))
    assert(isOLSmaller(IntT, top))
    assert(isOLSmaller(bot, IntT))
    assert(!isOLSmaller(top, IntT))
    assert(!isOLSmaller(IntT, bot))
  }

  test("OL: union / intersection") {
    assert(isOLSmaller(IntT, or(IntT, StringT)))
    assert(isOLSmaller(and(IntT, StringT), IntT))
  }

  test("OL: negation, excluded middle, contradiction") {
    assert(isSame(neg(neg(a)), a))
    assert(isSame(or(a, neg(a)), top))
    assert(isSame(and(a, neg(a)), bot))
  }

  test("OL: commutativity and idempotence") {
    assert(isSame(and(a, b), and(b, a)))
    assert(isSame(and(a, a), a))
  }

  test("OL: (a ∧ ¬a) = (b ∧ ¬b)") {
    assert(isSame(and(a, neg(a)), and(b, neg(b))))
  }

  test("OL: De Morgan 4-var") {
    assert(isSame(neg(or(x0, x1, x2, x3)), and(neg(x0), neg(x1), neg(x2), neg(x3))))
  }

  test("OL: absorption a ∨ (a ∧ (b ∨ (b ∧ c))) = a") {
    assert(isSame(or(a, and(a, or(b, and(b, c)))), a))
  }

  test("OL: distributivity one direction only") {
    assert(isOLSmaller(or(and(a, b), and(a, c)), and(a, or(b, c))))
    assert(!isOLSmaller(and(a, or(b, c)), or(and(a, b), and(a, c))))
  }

  test("OL: 8-variable excluded middle") {
    val conj = and(x0, x1, x2, x3, x4, x5, x6, x7)
    assert(isSame(or(conj, neg(conj)), top))
  }

  // =========================================================================
  // Covariant: List[+A]
  // =========================================================================

  test("covariant: List[⊥] <: List[Int] <: List[⊤]") {
    assert(isOLSmaller(mkList(bot), mkList(IntT)))
    assert(isOLSmaller(mkList(IntT), mkList(top)))
  }

  test("covariant: List[A] ≮: List[B]") {
    assert(!isOLSmaller(mkList(a), mkList(b)))
  }

  test("covariant: List[Int] <: List[Int | String]") {
    assert(isOLSmaller(mkList(IntT), mkList(or(IntT, StringT))))
  }

  // =========================================================================
  // Contravariant: Consumer[-A]
  // =========================================================================

  test("contravariant: Consumer[⊤] <: Consumer[Int] <: Consumer[⊥]") {
    assert(isOLSmaller(mkConsumer(top), mkConsumer(IntT)))
    assert(isOLSmaller(mkConsumer(IntT), mkConsumer(bot)))
  }

  test("contravariant: Consumer[Int] ≮: Consumer[⊤]") {
    assert(!isOLSmaller(mkConsumer(IntT), mkConsumer(top)))
  }

  // =========================================================================
  // Binary covariant: Pair[+A, +B]
  // =========================================================================

  test("pair: Pair[⊥, Int] <: Pair[Int, Int] <: Pair[⊤, ⊤]") {
    assert(isOLSmaller(mkPair(bot, IntT), mkPair(IntT, IntT)))
    assert(isOLSmaller(mkPair(IntT, IntT), mkPair(top, top)))
  }

  test("pair: Pair[Int, String] ≮: Pair[String, Int]") {
    assert(!isOLSmaller(mkPair(IntT, StringT), mkPair(StringT, IntT)))
  }

  // =========================================================================
  // Mixed variance: Func[-A, +B]
  // =========================================================================

  test("func: Func[⊤, ⊥] <: Func[Int, Int]") {
    assert(isOLSmaller(mkFunc(top, bot), mkFunc(IntT, IntT)))
  }

  test("func: Func[Int, Int] <: Func[Int, ⊤]") {
    assert(isOLSmaller(mkFunc(IntT, IntT), mkFunc(IntT, top)))
  }

  test("func: Func[Int, Int] ≮: Func[⊤, Int]") {
    assert(!isOLSmaller(mkFunc(IntT, IntT), mkFunc(top, IntT)))
  }

  // =========================================================================
  // Nested constructors
  // =========================================================================

  test("nested: List[List[⊥]] <: List[List[Int]] <: List[List[⊤]]") {
    assert(isOLSmaller(mkList(mkList(bot)), mkList(mkList(IntT))))
    assert(isOLSmaller(mkList(mkList(IntT)), mkList(mkList(top))))
  }

  test("nested: (Int→Int)→Int <: (⊤→⊥)→Int") {
    assert(isOLSmaller(mkFunc(mkFunc(IntT, IntT), IntT), mkFunc(mkFunc(top, bot), IntT)))
  }

  // =========================================================================
  // Constructors with ∨/∧
  // =========================================================================

  test("List[Int & String] <: List[Int]") {
    assert(isOLSmaller(mkList(and(IntT, StringT)), mkList(IntT)))
  }

  test("List[Int] | List[String] <: List[Int | String]") {
    assert(isOLSmaller(or(mkList(IntT), mkList(StringT)), mkList(or(IntT, StringT))))
  }

  // =========================================================================
  // Negation of constructors
  // =========================================================================

  test("¬¬List[a] = List[a]") {
    assert(isSame(neg(neg(mkList(a))), mkList(a)))
  }

  test("List[a] | ¬List[a] = ⊤") {
    assert(isSame(or(mkList(a), neg(mkList(a))), top))
  }

  test("List[a] & ¬List[a] = ⊥") {
    assert(isSame(and(mkList(a), neg(mkList(a))), bot))
  }

  // =========================================================================
  // Wide constructors: Tuple5 (5-ary +)
  // =========================================================================

  test("Tuple5: ⊥ args <: Int args <: ⊤ args") {
    assert(isOLSmaller(mkTuple5(bot, bot, bot, bot, bot), mkTuple5(IntT, IntT, IntT, IntT, IntT)))
    assert(isOLSmaller(mkTuple5(IntT, IntT, IntT, IntT, IntT), mkTuple5(top, top, top, top, top)))
  }

  test("Tuple5: single wrong direction fails") {
    assert(!isOLSmaller(mkTuple5(top, a, b, c, d), mkTuple5(IntT, a, b, c, d)))
  }

  test("Tuple5 | ¬Tuple5 = ⊤") {
    val t = mkTuple5(a, b, c, d, e)
    assert(isSame(or(t, neg(t)), top))
  }

  test("Tuple5 with union of constructors") {
    val t1 = mkTuple5(IntT, a, b, c, d)
    val t2 = mkTuple5(StringT, a, b, c, d)
    assert(isOLSmaller(or(t1, t2), mkTuple5(or(IntT, StringT), a, b, c, d)))
  }

  // =========================================================================
  // Wide constructors: Record6 (6-ary +−+−+−)
  // =========================================================================

  test("Record6: all positions correct variance") {
    assert(isOLSmaller(
      mkRecord6(IntT, top, IntT, top, IntT, top),
      mkRecord6(top, IntT, top, IntT, top, IntT)))
  }

  test("Record6: covariant position wrong direction fails") {
    assert(!isOLSmaller(mkRecord6(top, a, b, c, d, e), mkRecord6(IntT, a, b, c, d, e)))
  }

  test("Record6: contravariant position wrong direction fails") {
    assert(!isOLSmaller(mkRecord6(a, IntT, b, c, d, e), mkRecord6(a, top, b, c, d, e)))
  }

  // =========================================================================
  // Wide constructors: Alt8 (8-ary +−+−+−+−)
  // =========================================================================

  test("Alt8: all positions correct variance") {
    assert(isOLSmaller(
      mkAlt8(IntT, top, IntT, top, IntT, top, IntT, top),
      mkAlt8(top, IntT, top, IntT, top, IntT, top, IntT)))
  }

  test("Alt8: ⊥/⊤ args <: mixed base types") {
    assert(isOLSmaller(
      mkAlt8(bot, top, bot, top, bot, top, bot, top),
      mkAlt8(IntT, StringT, BoolT, NumberT, IntT, StringT, BoolT, NumberT)))
  }

  // =========================================================================
  // Nested wide constructors
  // =========================================================================

  test("List[Tuple5[Int...]] <: List[Tuple5[⊤...]]") {
    assert(isOLSmaller(
      mkList(mkTuple5(IntT, IntT, IntT, IntT, IntT)),
      mkList(mkTuple5(top, top, top, top, top))))
  }

  test("Tuple5[List[Int]...] <: Tuple5[List[⊤]...]") {
    assert(isOLSmaller(
      mkTuple5(mkList(IntT), mkList(IntT), mkList(IntT), mkList(IntT), mkList(IntT)),
      mkTuple5(mkList(top), mkList(top), mkList(top), mkList(top), mkList(top))))
  }

  test("Tuple5[Func[⊤,Int],...] <: Tuple5[Func[Int,⊤],...]") {
    assert(isOLSmaller(
      mkTuple5(mkFunc(top, IntT), a, b, c, d),
      mkTuple5(mkFunc(IntT, top), a, b, c, d)))
  }

  // =========================================================================
  // Different constructors don't relate
  // =========================================================================

  test("different constructors don't relate") {
    assert(!isOLSmaller(mkTuple5(a, b, c, d, e), mkTuple7(a, b, c, d, e, f, g)))
    assert(!isOLSmaller(mkTuple7(a, b, c, d, e, f, g), mkTuple5(a, b, c, d, e)))
  }

  // =========================================================================
  // Deep absorption towers
  // =========================================================================

  test("absorption tower 5-deep: a ∨ (a ∧ (b ∨ (b ∧ (c ∨ (c ∧ (d ∨ (d ∧ e))))))) = a") {
    assert(isSame(
      or(a, and(a, or(b, and(b, or(c, and(c, or(d, and(d, e)))))))),
      a))
  }

  test("dual absorption tower: a ∧ (a ∨ (b ∧ (b ∨ (c ∧ (c ∨ d))))) = a") {
    assert(isSame(
      and(a, or(a, and(b, or(b, and(c, or(c, d)))))),
      a))
  }

  test("multi-branch absorption: a ∨ (a ∧ b) ∨ (a ∧ c) ∨ (a ∧ d ∧ e) = a") {
    assert(isSame(or(a, and(a, b), and(a, c), and(a, d, e)), a))
  }

  // =========================================================================
  // Complex De Morgan
  // =========================================================================

  test("deep De Morgan: ¬(a ∧ (b ∨ (c ∧ d))) = ¬a ∨ (¬b ∧ (¬c ∨ ¬d))") {
    assert(isSame(
      neg(and(a, or(b, and(c, d)))),
      or(neg(a), and(neg(b), or(neg(c), neg(d))))))
  }

  test("De Morgan 5-var: ¬(a ∧ b ∧ c ∧ d ∧ e) = ¬a ∨ ¬b ∨ ¬c ∨ ¬d ∨ ¬e") {
    assert(isSame(
      neg(and(a, b, c, d, e)),
      or(neg(a), neg(b), neg(c), neg(d), neg(e))))
  }

  test("De Morgan nested: ¬((a ∨ b) ∧ (c ∨ d)) = (¬a ∧ ¬b) ∨ (¬c ∧ ¬d)") {
    assert(isSame(
      neg(and(or(a, b), or(c, d))),
      or(and(neg(a), neg(b)), and(neg(c), neg(d)))))
  }

  // =========================================================================
  // Generalized excluded middle / contradiction
  // =========================================================================

  test("(a ∧ b) ∨ ¬a ∨ ¬b = ⊤  (since ¬a ∨ ¬b = ¬(a ∧ b))") {
    assert(isSame(or(and(a, b), neg(a), neg(b)), top))
  }

  test("(a ∧ b ∧ c ∧ d) ∨ ¬a ∨ ¬b ∨ ¬c ∨ ¬d = ⊤") {
    assert(isSame(or(and(a, b, c, d), neg(a), neg(b), neg(c), neg(d)), top))
  }

  test("(a ∨ b) ∧ ¬a ∧ ¬b = ⊥  (since ¬a ∧ ¬b = ¬(a ∨ b))") {
    assert(isSame(and(or(a, b), neg(a), neg(b)), bot))
  }

  test("(a ∨ b ∨ c ∨ d) ∧ ¬a ∧ ¬b ∧ ¬c ∧ ¬d = ⊥") {
    assert(isSame(and(or(a, b, c, d), neg(a), neg(b), neg(c), neg(d)), bot))
  }

  // =========================================================================
  // One-sided lattice distributivity (always valid in any lattice)
  // =========================================================================

  test("one-sided: a ∨ (b ∧ c) ≤ (a ∨ b) ∧ (a ∨ c)") {
    assert(isOLSmaller(or(a, and(b, c)), and(or(a, b), or(a, c))))
  }

  test("one-sided 2×2: (a∧b) ∨ (c∧d) ≤ (a∨c) ∧ (b∨d)") {
    assert(isOLSmaller(or(and(a, b), and(c, d)), and(or(a, c), or(b, d))))
  }

  test("one-sided 3×2: (a∧b) ∨ (c∧d) ∨ (e∧f) ≤ (a∨c∨e) ∧ (b∨d∨f)") {
    assert(isOLSmaller(
      or(and(a, b), and(c, d), and(e, f)),
      and(or(a, c, e), or(b, d, f))))
  }

  test("biconditional one-sided: (a∧b) ∨ (¬a∧¬b) ≤ (a∨¬b) ∧ (¬a∨b)") {
    assert(isOLSmaller(
      or(and(a, b), and(neg(a), neg(b))),
      and(or(a, neg(b)), or(neg(a), b))))
  }

  test("sub-absorption: (a∧b) ∨ (a∧c) ≤ a ∧ ((a∧b) ∨ c)") {
    assert(isOLSmaller(
      or(and(a, b), and(a, c)),
      and(a, or(and(a, b), c))))
  }

  // =========================================================================
  // Negative: distributivity & Boolean-but-not-OL failures
  // =========================================================================

  test("distrib reverse FAILS: (a∨b) ∧ (a∨c) ≰ a ∨ (b∧c)") {
    assert(!isOLSmaller(and(or(a, b), or(a, c)), or(a, and(b, c))))
  }

  test("meet-distrib reverse FAILS: a ∧ (b∨c) ≰ (a∧b) ∨ (a∧c)") {
    assert(!isOLSmaller(and(a, or(b, c)), or(and(a, b), and(a, c))))
  }

  test("case split FAILS: a ≰ (a∧b) ∨ (a∧¬b)") {
    assert(!isOLSmaller(a, or(and(a, b), and(a, neg(b)))))
  }

  test("dual case split FAILS: (a∨b) ∧ (a∨¬b) ≰ a") {
    assert(!isOLSmaller(and(or(a, b), or(a, neg(b))), a))
  }

  test("orthomodular law FAILS: a∨b ≰ a ∨ (¬a ∧ (a∨b))") {
    assert(!isOLSmaller(or(a, b), or(a, and(neg(a), or(a, b)))))
  }

  test("Boolean partition FAILS: ⊤ ≰ (a∧b) ∨ (a∧¬b) ∨ (¬a∧b) ∨ (¬a∧¬b)") {
    assert(!isOLSmaller(
      top,
      or(and(a, b), and(a, neg(b)), and(neg(a), b), and(neg(a), neg(b)))))
  }

  test("biconditional reverse FAILS: (a∨¬b) ∧ (¬a∨b) ≰ (a∧b) ∨ (¬a∧¬b)") {
    assert(!isOLSmaller(
      and(or(a, neg(b)), or(neg(a), b)),
      or(and(a, b), and(neg(a), neg(b)))))
  }

  test("sub-absorption reverse FAILS: a ∧ ((a∧b) ∨ c) ≰ (a∧b) ∨ (a∧c)") {
    assert(!isOLSmaller(
      and(a, or(and(a, b), c)),
      or(and(a, b), and(a, c))))
  }

  // =========================================================================
  // Multi-variable positive inequalities
  // =========================================================================

  test("weakening: a ∧ b ∧ c ≤ a ∨ d ∨ e") {
    assert(isOLSmaller(and(a, b, c), or(a, d, e)))
  }

  test("monotone meet: a ∧ b ≤ (a∨c) ∧ (b∨d)") {
    assert(isOLSmaller(and(a, b), and(or(a, c), or(b, d))))
  }

  test("resolution + weakening: a ∧ (¬a∨b) ∧ c ≤ b ∨ c") {
    assert(isOLSmaller(and(a, or(neg(a), b), c), or(b, c)))
  }

  test("large conjunction ≤ large disjunction sharing one var") {
    assert(isOLSmaller(
      and(a, b, c, d, e, f, g),
      or(a, h)))
  }

  test("nested meet-join: (a∧b∧c) ∨ (d∧e∧f) ≤ (a∨d) ∧ (b∨e) ∧ (c∨f)") {
    assert(isOLSmaller(
      or(and(a, b, c), and(d, e, f)),
      and(or(a, d), or(b, e), or(c, f))))
  }

  // =========================================================================
  // Complex non-trivial OL equivalences
  // =========================================================================

  test("double negation deep: ¬¬(a ∧ (b ∨ ¬c)) = a ∧ (b ∨ ¬c)") {
    assert(isSame(neg(neg(and(a, or(b, neg(c))))), and(a, or(b, neg(c)))))
  }

  test("(a∧b) ∨ (¬a∧b) ≠ b  (both ≤ b, but not the reverse)") {
    assert(isOLSmaller(or(and(a, b), and(neg(a), b)), b))
    assert(!isOLSmaller(b, or(and(a, b), and(neg(a), b))))
  }

  test("self-dual absorption: (a ∨ (b ∧ a)) ∧ (a ∨ c) = a ∧ (a ∨ c) = a") {
    assert(isSame(and(or(a, and(b, a)), or(a, c)), a))
  }

  test("triple excluded middle: (a∧b∧c) ∨ ¬a ∨ ¬b ∨ ¬c = ⊤") {
    assert(isSame(or(and(a, b, c), neg(a), neg(b), neg(c)), top))
  }
}
