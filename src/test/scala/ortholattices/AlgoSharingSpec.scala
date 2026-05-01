package ortholattices

import munit.FunSuite
import ortholattices.algorithms.Datastructures.*
import ortholattices.algorithms.{OLAlgorithm, OLAlgorithmStructural, Printer}

/**
 * Investigates how each algorithm handles formulas with heavy structure sharing
 * in the input — specifically chains of conjunctions where intermediate nodes
 * are reused at the root level.
 *
 * The core question raised by the user: does a flat-output algorithm (OLAlgorithm)
 * produce a larger output circuit than a tree-preserving one (OLAlgorithmStructural) when the
 * input exploits sharing?
 *
 * circuitSize semantics recap:
 *   - Neg is FREE (transparent)
 *   - And/Or with n children: Σ foldedSize(child_i) + (n-1) connectors
 *   - Shared object (same uniqueKey) counted only on first visit → 0 on repeats
 */
class AlgoSharingSpec extends FunSuite {

  // Helpers
  def cs(f: Formula): BigInt = circuitSize(List(f))
  def newA2    = new OLAlgorithmStructural
  def newOld   = new OLAlgorithm

  // Check logical equivalence via OL (used for soundness assertions).
  def equivalent(a: Formula, b: Formula): Boolean = OLAlgorithm.isSame(a, b)

  // Run a2 and old, return (a2Out, oldOut) along with their circuit sizes.
  def runAll(f: Formula): ((Formula, Formula), (BigInt, BigInt)) =
    val r2 = newA2.reducedForm(f)
    val r4 = newOld.reducedForm(f)
    ((r2, r4), (cs(r2), cs(r4)))

  // ── Case 1: absorbing chain (reduces to single branch) ──────────────────
  //
  // c1 = x2 ∧ x3
  // c2 = x1 ∧ c1        (reuses c1, same object)
  // c3 = x0 ∧ c2        (reuses c2 which reuses c1)
  // root = c3 ∧ c2 ∧ c1 (root directly reuses all three)
  //
  // OL absorption: c3 ≤ c2 ≤ c1 (c3 implies c2 implies c1)
  //   → root normalizes to c3 = x0 ∧ x1 ∧ x2 ∧ x3
  //
  // Input circuit size: c1(3) is shared 3× → 9 total.
  // After normalization the result is a 4-variable conjunction.
  // Flat And([x0,x1,x2,x3]) has circuitSize = 4 + 3 = 7.
  // Nested And([x0, And([x1, And([x2, x3])])]) also has circuitSize = 7.
  // (Nesting doesn't help here because there is no repeated sub-expression in
  //  the *output* — the output is just a linear chain of 4 distinct leaves.)

  test("chain: input has sharing, output is a flat/nested 4-var conjunction") {
    val c1   = And(List(Variable(2), Variable(3)))
    val c2   = And(List(Variable(1), c1))       // c1 reused (same object)
    val c3   = And(List(Variable(0), c2))       // c2 (and through it c1) reused
    val root = And(List(c3, c2, c1))            // all three reused at root

    // Input DAG exploits sharing: c1 appears 3×, c2 appears 2× → total 9.
    assertEquals(cs(root), BigInt(9),
      "input should be 9 (sharing collapses duplicate visits)")

    val ((r_a2, r_old), (sz_a2, sz_old)) = runAll(root)

    // All must produce equivalent results.
    assert(equivalent(r_a2,  r_old), s"a2  \u2241 old:\n  a2 =${Printer.pretty(r_a2)}\n  old=${Printer.pretty(r_old)}")

    // The normalized formula is x0 ∧ x1 ∧ x2 ∧ x3 (4 leaves, 3 connectors = 7).
    // Flat or nested, any arrangement of 4 distinct variable leaves gives 7.
    assertEquals(sz_a2,  BigInt(7), s"a2  output size: ${Printer.pretty(r_a2)}")
    assertEquals(sz_old, BigInt(7), s"old output size: ${Printer.pretty(r_old)}")
  }

  // ── Case 2: shared positive conjunction in Or branches ──────────────────
  //
  // inner = And([x0, x1])   ← same Formula object
  // root  = Or([And([inner, x2]), And([inner, x3])])
  //
  // OL cannot simplify this (x0∧x1∧x2 and x0∧x1∧x3 are incomparable).
  // The normalized output is logically Or([And([x0,x1,x2]), And([x0,x1,x3])]).
  //
  // Since inner is a positive-polarity PolarAnd, the key question is whether
  // the algorithms preserve sharing of the normalized inner And across branches.
  //
  // With the buildTransparentTree cache fix: inner's NormalPFormula is memoized
  // after the first branch normalizes it, so the second branch reuses the same
  // object → toFormulaNNF shares the And([x0,x1]) Formula → circuit size 8.
  //
  // OLAlgorithm (flat, no fix): each branch gets its own flat And object,
  // only variable leaves are shared → circuit size 9.
  //
  // Expected circuit size with sharing:
  //   Or([And([inner_shared, x2]), And([inner_shared, x3])])
  //     inner(3)+x2(1)+And-conn(1)=5 ; inner(0)+x3(1)+And-conn(1)=2 ; Or-conn(1) = 8

  test("shared positive inner-And in Or branches: new/a2 share inner And") {
    val inner = And(List(Variable(0), Variable(1)))   // same object
    val root  = Or(List(And(List(inner, Variable(2))), And(List(inner, Variable(3)))))

    val ((r_a2, r_old), (sz_a2, sz_old)) = runAll(root)

    assert(equivalent(r_a2,  r_old))

    // a2: inner And([x0,x1]) is shared between branches → size 8.
    assertEquals(sz_a2,  BigInt(8), s"a2 : ${Printer.pretty(r_a2)}")
    // Old (flat): no sharing of inner And → size 9.
    assertEquals(sz_old, BigInt(9), s"old: ${Printer.pretty(r_old)}")
  }

  // ── Case 3: shared NEGATIVE conjunction (disjunction) in And branches ───
  //
  // This is the case where sharing IS preserved in the normalized output.
  // A disjunction `Or([x0, x1])` has polarity=false in NNF.
  // buildTransparentTree(PolarAnd(_, false)) calls nPnormalForm, which IS memoized.
  // → Both branches in the output share the SAME NPAnd(_, false) object
  // → toFormulaNNF returns the SAME Or([x0,x1]) Formula object for both
  // → circuitSize counts it only once.
  //
  // neg_inner = Or([x0, x1])   ← same Formula object
  // root = And([And([neg_inner, x2]), And([neg_inner, x3])])
  //       = neg_inner ∧ x2 ∧ neg_inner ∧ x3
  //       = neg_inner ∧ x2 ∧ x3   (idempotence removes second neg_inner)
  //
  // After normalization: And([neg_inner, x2, x3]) or equivalent.
  // Expected output circuit size:
  //   neg_inner = Or([x0,x1]) costs 3
  //   And([neg_inner, x2, x3]): 3+1+1+2 = 7 (flat) or 3+1+1+1+1 = 7 (nested)
  //   All algorithms: 7.

  test("shared negative inner (disjunction Or([x0,x1])) in And: all output size 7") {
    val neg_inner = Or(List(Variable(0), Variable(1)))   // same object
    val root = And(List(And(List(neg_inner, Variable(2))), And(List(neg_inner, Variable(3)))))

    val ((r_a2, r_old), (sz_a2, sz_old)) = runAll(root)

    assert(equivalent(r_a2,  r_old))

    assertEquals(sz_a2,  BigInt(7), s"a2 : ${Printer.pretty(r_a2)}")
    assertEquals(sz_old, BigInt(7), s"old: ${Printer.pretty(r_old)}")
  }

  // ── Case 4: Or of two And branches sharing a DISJUNCTION (Or) sub-expression ──
  //
  // This is the one case where output sharing *could* differ between algorithms.
  // neg_inner = Or([x0, x1])  ← same object (polarity=false in NNF)
  // root = Or([And([neg_inner, x2]), And([neg_inner, x3])])
  //      = (neg_inner ∧ x2) ∨ (neg_inner ∧ x3)
  //
  // OL cannot simplify this (incomparable branches).
  // The normalized output preserves neg_inner in both branches.
  //
  // Because neg_inner is non-transparent (polarity=false), buildTransparentTree
  // calls nPnormalForm on it, which IS memoized.  Both branches in ALL algorithms
  // reference the SAME NormalPFormula object for neg_inner → toFormulaNNF
  // returns the SAME Or([x0,x1]) Formula object for both → circuitSize counts
  // Or([x0,x1]) only once.
  //
  // Expected circuit size for output Or([And([neg_inner, x2]), And([neg_inner, x3])]):
  //   neg_inner Or([x0,x1]) first visit: x0(1)+x1(1)+conn(1) = 3
  //   And([neg_inner, x2]): 3+1+1 = 5   (first branch)
  //   neg_inner second visit: 0 (seen)
  //   And([neg_inner, x3]): 0+1+1 = 2   (second branch)
  //   Outer Or: 5+2+1 = 8
  //
  // All three algorithms should give 8 (because NormalPFormula memoization is
  // shared infrastructure — the same NPAnd object for neg_inner is reused by all).

  test("Or of two And-branches sharing a disjunction sub-expression: all output size 8") {
    val neg_inner = Or(List(Variable(0), Variable(1)))   // same object
    val root = Or(List(And(List(neg_inner, Variable(2))), And(List(neg_inner, Variable(3)))))

    val ((r_a2, r_old), (sz_a2, sz_old)) = runAll(root)

    assert(equivalent(r_a2,  r_old))

    // neg_inner (Or([x0,x1])) is shared as a Formula object in ALL outputs
    // → second occurrence costs 0 → total 8.
    assertEquals(sz_a2,  BigInt(8), s"a2 : ${Printer.pretty(r_a2)}")
    assertEquals(sz_old, BigInt(8), s"old: ${Printer.pretty(r_old)}")
  }

  // ── Case 5: the flat-vs-tree difference for CONNECTOR count ─────────────
  //
  // For any conjunction of n distinct leaves the formula:
  //   flat:   And([l0, l1, ..., l_{n-1}])        costs  n + (n-1) = 2n-1
  //   nested: And([l0, And([l1, And([...])])])    costs  2n-1  (same!)
  //
  // Nesting alone never reduces circuit size when there is no repeated
  // sub-expression object.  This test makes that explicit.

  test("flat vs nested conjunction of 5 distinct leaves: same circuit size") {
    val leaves = (0 until 5).map(Variable(_)).toList
    val flat   = And(leaves)
    val nested = leaves.tail.foldRight(leaves.head: Formula)((l, acc) => And(List(l, acc)))

    assertEquals(cs(flat),   BigInt(9))  // 5 + 4 = 9
    assertEquals(cs(nested), BigInt(9))  // also 2*5-1 = 9
  }

  // ── Case 6: large chain with wide sharing ────────────────────────────────
  //
  // Build c_0 = x_0 ∧ x_1 ∧ ... ∧ x_{n-1} step-by-step as a right-associative
  // chain so that each c_i is the same Formula object used in the next level.
  // Then reference ALL intermediate nodes in the root And.
  //
  // After OL normalization: root = c_full (the largest conjunction, by absorption).
  // Output circuit size = 2*n - 1 for any algorithm (n = numVars leaves).

  test("large absorbing chain (n=6): all algorithms output circuit size 11") {
    val n = 6
    val vars = (0 until n).map(Variable(_)).toList
    // Build: c0=x0, c1=x0∧x1 (step by step, each ci is a new And that reuses ci-1)
    val nodes = vars.tail.scanLeft(vars.head: Formula)((acc, v) => And(List(acc, v)))
    // nodes = [x0, And([x0,x1]), And([And([x0,x1]),x2]), ..., And([...,x5])]
    val root = And(nodes)   // root references all intermediate nodes

    // Input circuit size: each node (except the last, which is pool.last) is
    // referenced once from inside the chain and once from root.  The leftmost
    // x0 is referenced from the chain multiple times → heavy sharing.
    // The input size is small compared to an unshared tree.
    val inputSize = cs(root)
    assert(inputSize < BigInt(2 * n * n),
      s"input should be much smaller than unshared tree (actual=$inputSize)")

    val ((r_a2, r_old), (sz_a2, sz_old)) = runAll(root)

    assert(equivalent(r_a2,  r_old))

    // Output = x0 ∧ x1 ∧ ... ∧ x_{n-1}, circuit size = 2n-1 = 11.
    val expectedOut = BigInt(2 * n - 1)
    assertEquals(sz_a2,  expectedOut, s"a2 : ${Printer.pretty(r_a2)}")
    assertEquals(sz_old, expectedOut, s"old: ${Printer.pretty(r_old)}")
  }

  // ── Case 7: chain with shared nodes + redundant leaf (sharing-preservation test) ──
  //
  // c0 = x0, c1 = And(x0,x1), c2 = And(c1,x2), c3 = And(c2,x3) — shared chain
  // supnode = And(x0, c3) — x0 is redundant (already in c3)
  // root = Or(f1(c0), f2(c1), f3(c2), f4(c3), f5(supnode))
  //
  // After simplification, supnode → c3 (by absorption/idempotence of x0).
  // So f5(supnode) → f5(c3), sharing the c3 subtree with f4.
  //
  // OLAlgorithm/OLAlgorithmStructural (with sharing-preserving fix):
  //   Output preserves nesting and sharing → f4, f5 share c3_formula,
  //   f3 shares c2_formula (which contains c1_formula), etc.
  //   Circuit size = 16 for n=4.
  //
  // OLAlgorithm (always flat, no sharing):
  //   Each fi gets its own flat And, no shared sub-expressions.
  //   Circuit size = 22 for n=4.

  test("long chain with nodes reused: sharing preserved by new/a2") {
    val n = 4
    val vars = (0 until n).map(Variable(_)).toList
    val nodes = vars.tail.scanLeft(vars.head: Formula)((acc, v) => And(List(acc, v)))
    val supnode = And(List(vars.head, nodes.last))
    var i = 0
    def f(i: Int, f: Formula): Formula = FunApplication(FunSymbol(s"f$i", List(Variance.Invariant)), List(f))
    val root = Or(nodes.map(c => { i += 1; f(i, c) }) :+ f(i + 1, supnode))

    val inputSize = cs(root)
    val ((r_a2, r_old), (sz_a2, sz_old)) = runAll(root)

    assert(equivalent(r_a2, r_old))

    // a2 preserves sharing: f4 and f5 share the c3 subtree,
    // and the nested chain c1⊂c2⊂c3 is also shared.
    // Output circuit size < input (the redundant x0 was removed, saving cost).
    assert(sz_a2 < inputSize, s"a2 output ($sz_a2) should be < input ($inputSize)")
    assertEquals(sz_a2, BigInt(16), s"a2 : ${Printer.pretty(r_a2)}")

    // Old algorithm flattens everything → no sharing → larger output.
    assert(sz_old > inputSize, s"old output ($sz_old) should be > input ($inputSize) (no sharing)")
    assertEquals(sz_old, BigInt(22), s"old: ${Printer.pretty(r_old)}")
  }
}
