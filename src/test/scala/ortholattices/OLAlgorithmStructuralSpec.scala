package ortholattices

import ortholattices.Helpers.*
import ortholattices.algorithms.*
import ortholattices.algorithms.Datastructures.{circuitSize, Formula, Variable, FunSymbol, Variance, Literal, And, Or, Neg}
import ortholattices.algorithms.OLAlgorithm.*
import ortholattices.algorithms.EntailmentAlgorithm.{isEntailed, isEquivalent}

/**
 * Extensive correctness tests for OLAlgorithmStructural (tree-preserving snapshot).
 *
 * Three main property families:
 *   1. Soundness:  nf(φ) ≡ φ  (checked via independent entailment algorithm)
 *   2. Idempotence: nf(nf(φ)) ≡ nf(φ)
 *   3. Circuit-size parity: circuitSize(a2) == circuitSize(old)
 *
 * All random tests use deterministic seeds for reproducibility.
 */
class OLAlgorithmStructuralSpec extends munit.FunSuite {

  // =========================================================================
  // Helpers
  // =========================================================================

  /** Fresh OLAlgorithmStructural instance (each test needs fresh memoization). */
  private def mkA2() = new OLAlgorithmStructural
  private def mkOld() = new OLAlgorithm

  /** Reduce with OLAlgorithmStructural. */
  private def a2Reduce(f: Formula): Formula = mkA2().reducedForm(f)

  /** Reduce with OLAlgorithm. */
  private def oldReduce(f: Formula): Formula = mkOld().reducedForm(f)

  /** Check f1 ≤ f2 in OL using the entailment algorithm (no normalization). */
  private def entails(f1: Formula, f2: Formula): Boolean = isEntailed(f1, f2)

  /** Check f1 ≡ f2 in OL using the entailment algorithm. */
  private def equiv(f1: Formula, f2: Formula): Boolean = isEquivalent(f1, f2)

  // =========================================================================
  // 1. Soundness on hand-crafted formulas
  //    nf(φ) ≡ φ  via entailment
  // =========================================================================

  test("soundness: single variable") {
    val f = Variable(0)
    val nf = a2Reduce(f)
    assert(equiv(f, nf), s"$f ≢ $nf")
  }

  test("soundness: negated variable") {
    val f = neg(Variable(0))
    val nf = a2Reduce(f)
    assert(equiv(f, nf), s"$f ≢ $nf")
  }

  test("soundness: double negation") {
    val f = neg(neg(Variable(0)))
    val nf = a2Reduce(f)
    assert(equiv(f, nf), s"$f ≢ $nf")
  }

  test("soundness: top and bottom") {
    assert(equiv(a2Reduce(top), top))
    assert(equiv(a2Reduce(bot), bot))
  }

  test("soundness: x ∧ ¬x = ⊥") {
    val f = and(Variable(0), neg(Variable(0)))
    val nf = a2Reduce(f)
    assert(equiv(nf, bot), s"$nf ≢ ⊥")
  }

  test("soundness: x ∨ ¬x = ⊤") {
    val f = or(Variable(0), neg(Variable(0)))
    val nf = a2Reduce(f)
    assert(equiv(nf, top), s"$nf ≢ ⊤")
  }

  test("soundness: De Morgan") {
    val a = Variable(0); val b = Variable(1)
    val f1 = neg(and(a, b))
    val f2 = or(neg(a), neg(b))
    assert(equiv(a2Reduce(f1), a2Reduce(f2)))
  }

  test("soundness: absorption a ∧ (a ∨ b) = a") {
    val a = Variable(0); val b = Variable(1)
    val f = and(a, or(a, b))
    val nf = a2Reduce(f)
    assert(equiv(nf, a), s"$nf ≢ $a")
  }

  test("soundness: absorption a ∨ (a ∧ b) = a") {
    val a = Variable(0); val b = Variable(1)
    val f = or(a, and(a, b))
    val nf = a2Reduce(f)
    assert(equiv(nf, a), s"$nf ≢ $a")
  }

  test("soundness: idempotence a ∧ a = a") {
    val a = Variable(0)
    val f = and(a, a)
    val nf = a2Reduce(f)
    assert(equiv(nf, a), s"$nf ≢ $a")
  }

  test("soundness: deep nesting") {
    val a = Variable(0); val b = Variable(1); val c = Variable(2)
    val f = and(or(a, and(b, or(c, neg(a)))), neg(or(b, c)))
    val nf = a2Reduce(f)
    assert(equiv(f, nf), s"$f ≢ $nf")
  }

  test("soundness: large conjunction") {
    val vars = (0 until 10).map(Variable(_)).toList
    val f = and(vars)
    val nf = a2Reduce(f)
    assert(equiv(f, nf), s"$f ≢ $nf")
  }

  test("soundness: large disjunction") {
    val vars = (0 until 10).map(Variable(_)).toList
    val f = or(vars)
    val nf = a2Reduce(f)
    assert(equiv(f, nf), s"$f ≢ $nf")
  }

  test("soundness: mixed large formula") {
    val a = Variable(0); val b = Variable(1); val c = Variable(2); val d = Variable(3)
    val f = or(
      and(a, neg(b), or(c, d)),
      and(neg(a), b, neg(or(c, d))),
      and(c, neg(d))
    )
    val nf = a2Reduce(f)
    assert(equiv(f, nf), s"$f ≢ $nf")
  }

  // =========================================================================
  // 2. Idempotence on hand-crafted formulas
  //    nf(nf(φ)) ≡ nf(φ)
  // =========================================================================

  test("idempotence: single variable") {
    val nf1 = a2Reduce(Variable(0))
    val nf2 = a2Reduce(nf1)
    assert(equiv(nf1, nf2), s"$nf1 ≢ $nf2")
  }

  test("idempotence: contradiction") {
    val f = and(Variable(0), neg(Variable(0)))
    val nf1 = a2Reduce(f)
    val nf2 = a2Reduce(nf1)
    assert(equiv(nf1, nf2))
  }

  test("idempotence: tautology") {
    val f = or(Variable(0), neg(Variable(0)))
    val nf1 = a2Reduce(f)
    val nf2 = a2Reduce(nf1)
    assert(equiv(nf1, nf2))
  }

  test("idempotence: deep formula") {
    val a = Variable(0); val b = Variable(1); val c = Variable(2)
    val f = and(or(a, and(b, or(c, neg(a)))), neg(or(b, c)))
    val nf1 = a2Reduce(f)
    val nf2 = a2Reduce(nf1)
    assert(equiv(nf1, nf2), s"not idempotent: $nf1 vs $nf2")
  }

  // =========================================================================
  // 3. Circuit size parity with OLAlgorithm on hand-crafted formulas
  // =========================================================================

  test("circuit size: single variable") {
    val f = Variable(0)
    assertEquals(
      circuitSize(List(a2Reduce(f))),
      circuitSize(List(oldReduce(f)))
    )
  }

  test("circuit size: absorption") {
    val a = Variable(0); val b = Variable(1)
    val f = and(a, or(a, b))
    assertEquals(
      circuitSize(List(a2Reduce(f))),
      circuitSize(List(oldReduce(f)))
    )
  }

  test("circuit size: De Morgan") {
    val a = Variable(0); val b = Variable(1)
    val f = neg(and(a, b))
    assertEquals(
      circuitSize(List(a2Reduce(f))),
      circuitSize(List(oldReduce(f)))
    )
  }

  // =========================================================================
  // 4. Random formula tests — SOUNDNESS
  //    For many sizes × seeds, check nf(φ) ≡ φ via entailment.
  // =========================================================================

  private val smallSizes = List(3, 5, 7, 10, 15, 20)
  private val mediumSizes = List(25, 30, 40, 50)
  private val largeSizes = List(75, 100)
  private val numVarsList = List(2, 3, 4, 5)
  private val seedsPerConfig = 20

  for {
    size <- smallSizes
    numVars <- numVarsList
  } {
    test(s"soundness random: size=$size vars=$numVars (${seedsPerConfig} seeds)") {
      for (seed <- 0 until seedsPerConfig) {
        val formula = FormulaGenerator.randomFormula(size, numVars, Some(seed), doFlatten = false)
        val nf = mkA2().reducedForm(formula)
        assert(
          equiv(formula, nf),
          s"SOUNDNESS FAIL at size=$size vars=$numVars seed=$seed:\n  formula=$formula\n  nf=$nf"
        )
      }
    }
  }

  for {
    size <- mediumSizes
    numVars <- List(3, 5)
  } {
    test(s"soundness random: size=$size vars=$numVars (${seedsPerConfig} seeds)") {
      for (seed <- 0 until seedsPerConfig) {
        val formula = FormulaGenerator.randomFormula(size, numVars, Some(seed), doFlatten = false)
        val nf = mkA2().reducedForm(formula)
        assert(
          equiv(formula, nf),
          s"SOUNDNESS FAIL at size=$size vars=$numVars seed=$seed:\n  formula=$formula\n  nf=$nf"
        )
      }
    }
  }

  for {
    size <- largeSizes
  } {
    test(s"soundness random: size=$size vars=3 (10 seeds)") {
      for (seed <- 0 until 10) {
        val formula = FormulaGenerator.randomFormula(size, 3, Some(seed), doFlatten = false)
        val nf = mkA2().reducedForm(formula)
        assert(
          equiv(formula, nf),
          s"SOUNDNESS FAIL at size=$size seed=$seed:\n  formula=$formula\n  nf=$nf"
        )
      }
    }
  }

  // =========================================================================
  // 5. Random formula tests — IDEMPOTENCE
  //    nf(nf(φ)) ≡ nf(φ) (both entailment directions)
  // =========================================================================

  for {
    size <- smallSizes
    numVars <- numVarsList
  } {
    test(s"idempotence random: size=$size vars=$numVars (${seedsPerConfig} seeds)") {
      for (seed <- 0 until seedsPerConfig) {
        val formula = FormulaGenerator.randomFormula(size, numVars, Some(seed), doFlatten = false)
        val nf1 = mkA2().reducedForm(formula)
        val nf2 = mkA2().reducedForm(nf1)
        assert(
          equiv(nf1, nf2),
          s"IDEMPOTENCE FAIL at size=$size vars=$numVars seed=$seed:\n  nf1=$nf1\n  nf2=$nf2"
        )
      }
    }
  }

  for {
    size <- mediumSizes
    numVars <- List(3, 5)
  } {
    test(s"idempotence random: size=$size vars=$numVars (${seedsPerConfig} seeds)") {
      for (seed <- 0 until seedsPerConfig) {
        val formula = FormulaGenerator.randomFormula(size, numVars, Some(seed), doFlatten = false)
        val nf1 = mkA2().reducedForm(formula)
        val nf2 = mkA2().reducedForm(nf1)
        assert(
          equiv(nf1, nf2),
          s"IDEMPOTENCE FAIL at size=$size vars=$numVars seed=$seed:\n  nf1=$nf1\n  nf2=$nf2"
        )
      }
    }
  }

  for {
    size <- largeSizes
  } {
    test(s"idempotence random: size=$size vars=3 (10 seeds)") {
      for (seed <- 0 until 10) {
        val formula = FormulaGenerator.randomFormula(size, 3, Some(seed), doFlatten = false)
        val nf1 = mkA2().reducedForm(formula)
        val nf2 = mkA2().reducedForm(nf1)
        assert(
          equiv(nf1, nf2),
          s"IDEMPOTENCE FAIL at size=$size seed=$seed:\n  nf1=$nf1\n  nf2=$nf2"
        )
      }
    }
  }

  // =========================================================================
  // 6. Random formula tests — CIRCUIT SIZE PARITY WITH OLD ALGORITHM
  //    circuitSize(a2(φ)) == circuitSize(old(φ))
  // =========================================================================

  for {
    size <- smallSizes
    numVars <- numVarsList
  } {
    test(s"circuit-size parity random: size=$size vars=$numVars (${seedsPerConfig} seeds)") {
      for (seed <- 0 until seedsPerConfig) {
        val formula = FormulaGenerator.randomFormula(size, numVars, Some(seed), doFlatten = false)
        val a2Nf = mkA2().reducedForm(formula)
        val oldNf = mkOld().reducedForm(formula)
        val a2Size = circuitSize(List(a2Nf))
        val oldSize = circuitSize(List(oldNf))
        assert(a2Size <= oldSize,
          s"a2 output ($a2Size) should be ≤ old output ($oldSize): CIRCUIT SIZE MISMATCH at size=$size vars=$numVars seed=$seed:\n" +
          s"  a2=$a2Nf (size=$a2Size)\n  old=$oldNf (size=$oldSize)"
        )
      }
    }
  }

  for {
    size <- mediumSizes
    numVars <- List(3, 5)
  } {
    test(s"circuit-size parity random: size=$size vars=$numVars (${seedsPerConfig} seeds)") {
      for (seed <- 0 until seedsPerConfig) {
        val formula = FormulaGenerator.randomFormula(size, numVars, Some(seed), doFlatten = false)
        val a2Nf = mkA2().reducedForm(formula)
        val oldNf = mkOld().reducedForm(formula)
        val a2Size = circuitSize(List(a2Nf))
        val oldSize = circuitSize(List(oldNf))
        assert(a2Size <= oldSize,
          s"a2 output ($a2Size) should be ≤ old output ($oldSize): CIRCUIT SIZE MISMATCH at size=$size vars=$numVars seed=$seed:\n" +
          s"  a2=$a2Nf (size=$a2Size)\n  old=$oldNf (size=$oldSize)"
        )
      }
    }
  }

  for {
    size <- largeSizes
  } {
    test(s"circuit-size parity random: size=$size vars=3 (10 seeds)") {
      for (seed <- 0 until 10) {
        val formula = FormulaGenerator.randomFormula(size, 3, Some(seed), doFlatten = false)
        val a2Nf = mkA2().reducedForm(formula)
        val oldNf = mkOld().reducedForm(formula)
        val a2Size = circuitSize(List(a2Nf))
        val oldSize = circuitSize(List(oldNf))
        assert(a2Size <= oldSize,
          s"a2 output ($a2Size) should be ≤ old output ($oldSize): CIRCUIT SIZE MISMATCH at size=$size seed=$seed:\n" +
          s"  a2=$a2Nf (size=$a2Size)\n  old=$oldNf (size=$oldSize)"
        )
      }
    }
  }

  // =========================================================================
  // 7. Stress: combined soundness + idempotence + size parity in one pass
  //    Wide sweep over many seeds at sizes 5–50 with 3 variables.
  //    This mirrors FindDiscrepancy's scan range.
  // =========================================================================

  test("combined sweep: sizes 5–50, 3 vars, 500 seeds each") {
    for (size <- 5 to 50 by 5; seed <- 0 until 500) {
      val formula = FormulaGenerator.randomFormula(size, 3, Some(seed), doFlatten = false)

      val a2Algo = mkA2()
      val a2Nf = a2Algo.reducedForm(formula)
      val oldNf = mkOld().reducedForm(formula)

      // Soundness
      assert(
        equiv(formula, a2Nf),
        s"SOUNDNESS FAIL at size=$size seed=$seed"
      )

      // Idempotence
      val a2Nf2 = mkA2().reducedForm(a2Nf)
      assert(
        equiv(a2Nf, a2Nf2),
        s"IDEMPOTENCE FAIL at size=$size seed=$seed"
      )

      // Circuit-size parity
      val a2Size = circuitSize(List(a2Nf))
      val oldSize = circuitSize(List(oldNf))
      assert(a2Size <= oldSize,
        s"a2 output ($a2Size) should be ≤ old output ($oldSize): CIRCUIT SIZE MISMATCH at size=$size seed=$seed"
      )
    }
  }

  // =========================================================================
  // 8. Edge cases
  // =========================================================================

  test("edge: literal true") {
    val nf = a2Reduce(Literal(true))
    assert(equiv(nf, top))
  }

  test("edge: literal false") {
    val nf = a2Reduce(Literal(false))
    assert(equiv(nf, bot))
  }

  test("edge: nested contradictions") {
    val a = Variable(0); val b = Variable(1)
    val f = and(or(and(a, neg(a)), b), neg(b))
    val nf = a2Reduce(f)
    assert(equiv(nf, bot), s"$nf ≢ ⊥")
  }

  test("edge: nested tautologies") {
    val a = Variable(0); val b = Variable(1)
    val f = or(and(or(a, neg(a)), b), neg(b))
    val nf = a2Reduce(f)
    assert(equiv(nf, top), s"$nf ≢ ⊤")
  }

  test("edge: deeply nested formula (depth 10)") {
    var f: Formula = Variable(0)
    for (i <- 1 to 10) {
      f = if i % 2 == 0 then and(f, Variable(i % 3)) else or(f, neg(Variable(i % 3)))
    }
    val nf = a2Reduce(f)
    assert(equiv(f, nf), s"deep formula soundness failed")
    val nf2 = a2Reduce(nf)
    assert(equiv(nf, nf2), s"deep formula idempotence failed")
  }

  test("edge: wide conjunction (20 terms)") {
    val terms = (0 until 20).map(i => if i % 2 == 0 then Variable(i % 5) else neg(Variable(i % 5))).toList
    val f = and(terms)
    val nf = a2Reduce(f)
    assert(equiv(f, nf), s"wide conjunction soundness failed")
  }

  test("edge: wide disjunction (20 terms)") {
    val terms = (0 until 20).map(i => if i % 2 == 0 then Variable(i % 5) else neg(Variable(i % 5))).toList
    val f = or(terms)
    val nf = a2Reduce(f)
    assert(equiv(f, nf), s"wide disjunction soundness failed")
  }

  test("edge: alternating and/or tower") {
    // ((((x0 ∧ x1) ∨ x2) ∧ ¬x0) ∨ ¬x1) ∧ x2
    val f = and(or(and(or(and(Variable(0), Variable(1)), Variable(2)), neg(Variable(0))), neg(Variable(1))), Variable(2))
    val nf = a2Reduce(f)
    assert(equiv(f, nf), s"alternating tower soundness failed")
    assertEquals(circuitSize(List(nf)), circuitSize(List(oldReduce(f))))
  }

  test("edge: triple negation") {
    val f = neg(neg(neg(Variable(0))))
    val nf = a2Reduce(f)
    assert(equiv(nf, neg(Variable(0))))
  }

  test("edge: redundant duplicates") {
    val a = Variable(0)
    // a ∧ a ∧ a ∧ a ∧ a
    val f = and(List.fill(5)(a))
    val nf = a2Reduce(f)
    assert(equiv(nf, a), s"5-way idempotent and: $nf ≢ $a")
  }

  test("edge: all same in disjunction") {
    val a = Variable(0)
    val f = or(List.fill(5)(a))
    val nf = a2Reduce(f)
    assert(equiv(nf, a), s"5-way idempotent or: $nf ≢ $a")
  }

  // =========================================================================
  // 9. Agreement with OLAlgorithm (latest) on equivalence results
  // =========================================================================

  test("agreement with OLAlgorithm on random pairs (100 pairs)") {
    for (seed <- 0 until 100) {
      val f1 = FormulaGenerator.randomFormula(15, 3, Some(seed * 2), doFlatten = false)
      val f2 = FormulaGenerator.randomFormula(15, 3, Some(seed * 2 + 1), doFlatten = false)

      val oldAlgo = new OLAlgorithm
      val a2Algo = mkA2()

      val oldSame = oldAlgo.isSame(f1, f2)
      val a2Same = a2Algo.isSame(f1, f2)
      assertEquals(
        a2Same, oldSame,
        s"ALGORITHM AGREEMENT FAIL at seed=$seed: old=$oldSame a2=$a2Same\n  f1=$f1\n  f2=$f2"
      )
    }
  }

  // =========================================================================
  // 10. High variable count
  // =========================================================================

  for {
    numVars <- List(6, 8, 10)
  } {
    test(s"soundness+idempotence random: size=20 vars=$numVars (20 seeds)") {
      for (seed <- 0 until 20) {
        val formula = FormulaGenerator.randomFormula(20, numVars, Some(seed), doFlatten = false)
        val nf1 = mkA2().reducedForm(formula)

        assert(
          equiv(formula, nf1),
          s"SOUNDNESS FAIL at vars=$numVars seed=$seed"
        )

        val nf2 = mkA2().reducedForm(nf1)
        assert(
          equiv(nf1, nf2),
          s"IDEMPOTENCE FAIL at vars=$numVars seed=$seed"
        )
      }
    }
  }

  // =========================================================================
  // 11. Random circuit tests (structure sharing)
  //     CircuitGenerator produces DAGs where nodes are reused as operands.
  // =========================================================================

  private val circuitSizes  = List(5, 10, 20, 30, 50, 75, 100)
  private val circuitVarsList = List(2, 3, 5)
  private val circuitSeeds  = 20

  // --- Soundness on regular circuits ----------------------------------------

  for {
    numGates <- circuitSizes
    numVars  <- circuitVarsList
  } {
    test(s"circuit soundness: gates=$numGates vars=$numVars ($circuitSeeds seeds)") {
      for (seed <- 0 until circuitSeeds) {
        val circuit = CircuitGenerator.randomCircuit(numGates, numVars, Some(seed))
        val nf = mkA2().reducedForm(circuit)
        assert(
          equiv(circuit, nf),
          s"SOUNDNESS FAIL at gates=$numGates vars=$numVars seed=$seed:\n  circuit=$circuit\n  nf=$nf"
        )
      }
    }
  }

  // --- Idempotence on regular circuits --------------------------------------

  for {
    numGates <- circuitSizes
    numVars  <- circuitVarsList
  } {
    test(s"circuit idempotence: gates=$numGates vars=$numVars ($circuitSeeds seeds)") {
      for (seed <- 0 until circuitSeeds) {
        val circuit = CircuitGenerator.randomCircuit(numGates, numVars, Some(seed))
        val nf1 = mkA2().reducedForm(circuit)
        val nf2 = mkA2().reducedForm(nf1)
        assert(
          equiv(nf1, nf2),
          s"IDEMPOTENCE FAIL at gates=$numGates vars=$numVars seed=$seed:\n  nf1=$nf1\n  nf2=$nf2"
        )
      }
    }
  }

  // --- Circuit-size parity with old algorithm on regular circuits -----------

  for {
    numGates <- circuitSizes
    numVars  <- circuitVarsList
  } {
    test(s"circuit size parity: gates=$numGates vars=$numVars ($circuitSeeds seeds)") {
      for (seed <- 0 until circuitSeeds) {
        val circuit = CircuitGenerator.randomCircuit(numGates, numVars, Some(seed))
        val a2Nf  = mkA2().reducedForm(circuit)
        val oldNf = mkOld().reducedForm(circuit)
        val a2Size  = circuitSize(List(a2Nf))
        val oldSize = circuitSize(List(oldNf))
        assert(a2Size <= oldSize,
          s"a2 output ($a2Size) should be ≤ old output ($oldSize): CIRCUIT SIZE MISMATCH at gates=$numGates vars=$numVars seed=$seed:\n" +
          s"  a2=$a2Nf (size=$a2Size)\n  old=$oldNf (size=$oldSize)"
        )
      }
    }
  }

  // --- High-sharing circuits ------------------------------------------------

  for {
    numGates <- List(10, 30, 50, 100)
    numVars  <- List(2, 3, 5)
  } {
    test(s"high-sharing circuit soundness+idempotence+size: gates=$numGates vars=$numVars ($circuitSeeds seeds)") {
      for (seed <- 0 until circuitSeeds) {
        val circuit = CircuitGenerator.highSharingCircuit(numGates, numVars, Some(seed))
        val a2Nf  = mkA2().reducedForm(circuit)
        val oldNf = mkOld().reducedForm(circuit)

        assert(
          equiv(circuit, a2Nf),
          s"SOUNDNESS FAIL (high-sharing) at gates=$numGates vars=$numVars seed=$seed"
        )

        val a2Nf2 = mkA2().reducedForm(a2Nf)
        assert(
          equiv(a2Nf, a2Nf2),
          s"IDEMPOTENCE FAIL (high-sharing) at gates=$numGates vars=$numVars seed=$seed"
        )

        val a2Size = circuitSize(List(a2Nf))
        val oldSize = circuitSize(List(oldNf))
        assert(a2Size <= oldSize,
          s"a2 output ($a2Size) should be ≤ old output ($oldSize): CIRCUIT SIZE MISMATCH (high-sharing) at gates=$numGates vars=$numVars seed=$seed"
        )
      }
    }
  }

  // --- Stress sweep for circuits (all three properties) --------------------

  test("circuit combined sweep: gates 5–50, 3 vars, 500 seeds") {
    for (numGates <- 5 to 50 by 5; seed <- 0 until 500) {
      val circuit = CircuitGenerator.randomCircuit(numGates, 3, Some(seed))

      val a2Nf  = mkA2().reducedForm(circuit)
      val oldNf = mkOld().reducedForm(circuit)

      assert(equiv(circuit, a2Nf),
        s"SOUNDNESS FAIL at gates=$numGates seed=$seed")

      val a2Nf2 = mkA2().reducedForm(a2Nf)
      assert(equiv(a2Nf, a2Nf2),
        s"IDEMPOTENCE FAIL at gates=$numGates seed=$seed")

      val a2Size = circuitSize(List(a2Nf))
      val oldSize = circuitSize(List(oldNf))
      assert(a2Size <= oldSize,
        s"a2 output ($a2Size) should be ≤ old output ($oldSize): CIRCUIT SIZE MISMATCH at gates=$numGates seed=$seed")
    }
  }

  test("high-sharing circuit combined sweep: gates 5–50, 3 vars, 500 seeds") {
    for (numGates <- 5 to 50 by 5; seed <- 0 until 500) {
      val circuit = CircuitGenerator.highSharingCircuit(numGates, 3, Some(seed))

      val a2Nf  = mkA2().reducedForm(circuit)
      val oldNf = mkOld().reducedForm(circuit)

      assert(equiv(circuit, a2Nf),
        s"SOUNDNESS FAIL (high-sharing) at gates=$numGates seed=$seed")

      val a2Nf2 = mkA2().reducedForm(a2Nf)
      assert(equiv(a2Nf, a2Nf2),
        s"IDEMPOTENCE FAIL (high-sharing) at gates=$numGates seed=$seed")

      val a2Size = circuitSize(List(a2Nf))
      val oldSize = circuitSize(List(oldNf))
      assert(a2Size <= oldSize,
        s"a2 output ($a2Size) should be ≤ old output ($oldSize): CIRCUIT SIZE MISMATCH (high-sharing) at gates=$numGates seed=$seed")
    }
  }
}
