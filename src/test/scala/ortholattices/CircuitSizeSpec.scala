package ortholattices

import munit.FunSuite
import ortholattices.algorithms.Datastructures.*

/**
 * Tests for the `circuitSize` function.
 *
 * Semantics (from Datastructures.scala):
 *  - Variable / Literal          → 1  (leaf cost)
 *  - Neg(child)                  → foldedSize(child)  (Neg itself is FREE — no cost)
 *  - And(children) / Or(children) → sum(foldedSize(ci)) + (n - 1)   where n = arity
 *  - Shared nodes (same object / same uniqueKey) are counted only on their FIRST
 *    visit; subsequent visits return 0 (DAG sharing is exploited).
 *
 * Helper notation: `cs(f)` = circuitSize(List(f)).
 */
class CircuitSizeSpec extends FunSuite {

  // Helpers
  def cs(f: Formula): BigInt = circuitSize(List(f))
  def cs(fs: List[Formula]): BigInt = circuitSize(fs)

  // Fresh variables — each call produces a new object with a fresh uniqueKey
  def v(i: Int): Variable = Variable(i)

  // ── Leaves ────────────────────────────────────────────────────────────────

  test("leaf: Variable costs 1") {
    assertEquals(cs(v(0)), BigInt(1))
  }

  test("leaf: Literal(true) costs 1") {
    assertEquals(cs(Literal(true)), BigInt(1))
  }

  test("leaf: Literal(false) costs 1") {
    assertEquals(cs(Literal(false)), BigInt(1))
  }

  // ── Neg is free ───────────────────────────────────────────────────────────

  test("Neg(variable) has same cost as the variable") {
    val x = v(0)
    assertEquals(cs(Neg(x)), cs(x))
    assertEquals(cs(Neg(x)), BigInt(1))
  }

  test("double-Neg is still free (cost = 1)") {
    val x = v(0)
    assertEquals(cs(Neg(Neg(x))), BigInt(1))
  }

  test("Neg of an And: cost equals cost of the And itself") {
    val x = v(0); val y = v(1)
    val a = And(List(x, y))
    // And([x,y]) = 1+1+(2-1) = 3
    assertEquals(cs(Neg(a)), cs(a))
    assertEquals(cs(Neg(a)), BigInt(3))
  }

  // ── Binary And / Or (arity 2, cost = 1+1+1 = 3) ──────────────────────────

  test("binary And([x0, x1]) costs 3") {
    assertEquals(cs(And(List(v(0), v(1)))), BigInt(3))
  }

  test("binary Or([x0, x1]) costs 3") {
    assertEquals(cs(Or(List(v(0), v(1)))), BigInt(3))
  }

  // ── Multi-ary And / Or: cost = n_leaves + (n_children - 1) ───────────────

  test("3-ary And([x0,x1,x2]) costs 5") {
    // 1+1+1 + (3-1) = 5
    assertEquals(cs(And(List(v(0), v(1), v(2)))), BigInt(5))
  }

  test("3-ary Or([x0,x1,x2]) costs 5") {
    assertEquals(cs(Or(List(v(0), v(1), v(2)))), BigInt(5))
  }

  test("4-ary And([x0,x1,x2,x3]) costs 7") {
    // 1+1+1+1 + (4-1) = 7
    assertEquals(cs(And(List(v(0), v(1), v(2), v(3)))), BigInt(7))
  }

  test("5-ary Or([x0,x1,x2,x3,x4]) costs 9") {
    // 1*5 + (5-1) = 9
    assertEquals(cs(Or(List(v(0), v(1), v(2), v(3), v(4)))), BigInt(9))
  }

  test("3-ary And with a Neg child: Neg is free so cost stays 5") {
    // And([x0, Neg(x1), x2]): Neg(x1) contributes same as x1 = 1
    // 1+1+1+(3-1) = 5
    assertEquals(cs(And(List(v(0), Neg(v(1)), v(2)))), BigInt(5))
  }

  // ── Nested formulas (no sharing) ──────────────────────────────────────────

  test("nested: Or([And([x0,x1]), x2]) costs 5") {
    // And([x0,x1]) = 3;  Or(3-node-result, x2) = 3+1+(2-1) = 5
    assertEquals(cs(Or(List(And(List(v(0), v(1))), v(2)))), BigInt(5))
  }

  test("nested: And([Or([x0,x1]), Or([x2,x3])]) costs 7") {
    // Or([x0,x1]) = 3;  Or([x2,x3]) = 3;  And = 3+3+(2-1) = 7
    assertEquals(cs(And(List(Or(List(v(0), v(1))), Or(List(v(2), v(3)))))), BigInt(7))
  }

  test("deep nesting: Or([And([Or([x0,x1]), x2]), x3]) costs 8") {
    // inner Or([x0,x1]) = 3
    // And([3-node, x2]) = 3+1+(2-1) = 5
    // outer Or([5-node, x3]) = 5+1+(2-1) = 7  … wait, x3 = 1
    // = 5 + 1 + 1 = 7
    val inner = Or(List(v(0), v(1)))    // 3
    val mid   = And(List(inner, v(2)))  // 3+1+1 = 5
    val outer = Or(List(mid, v(3)))     // 5+1+1 = 7
    assertEquals(cs(outer), BigInt(7))
  }

  // ── Shared nodes (same object referenced multiple times) ──────────────────

  test("sharing: same variable object used twice in Or costs 2 (not 3)") {
    val x = v(0)
    // Or([x, x]): first visit x→1, second visit x→0 (seen); +1 = 2
    assertEquals(cs(Or(List(x, x))), BigInt(2))
  }

  test("sharing: same variable object used three times in 3-ary And costs 3 (not 5)") {
    val x = v(0)
    // And([x,x,x]): x→1, x→0, x→0; +(3-1) = 3
    assertEquals(cs(And(List(x, x, x))), BigInt(3))
  }

  test("sharing: inner And node reused twice by outer Or costs 4 (not 7)") {
    val x = v(0); val y = v(1)
    val inner = And(List(x, y))          // 3
    val outer = Or(List(inner, inner))   // 3+0+(2-1) = 4
    assertEquals(cs(outer), BigInt(4))
  }

  test("sharing: inner Or node reused twice by outer And costs 4 (not 7)") {
    val x = v(0); val y = v(1)
    val inner = Or(List(x, y))
    val outer = And(List(inner, inner))
    assertEquals(cs(outer), BigInt(4))
  }

  test("sharing: inner And node reused three times in 3-ary Or costs 5 (not 11)") {
    val x = v(0); val y = v(1)
    val inner = And(List(x, y))                    // 3
    val outer = Or(List(inner, inner, inner))       // 3+0+0+(3-1) = 5
    assertEquals(cs(outer), BigInt(5))
  }

  test("sharing: shared node at two levels of a DAG") {
    // a = And([x0, x1])            cost 3
    // b = Or([a, x2])              cost 3+1+(2-1) = 5  (a seen first here)
    // c = And([a, b])              a already seen → 0; b = 5; +(2-1) = 1 → total 6
    val x0 = v(0); val x1 = v(1); val x2 = v(2)
    val a = And(List(x0, x1))
    val b = Or(List(a, x2))
    val c = And(List(a, b))
    assertEquals(cs(c), BigInt(6))
  }

  test("sharing: Neg of shared node is also free and shares the same child cost") {
    // s = And([x0,x1])   cost 3
    // Or([s, Neg(s)]): foldedSize(s)=3 (marks s), foldedSize(Neg(s))→foldedSize(s)=0 (seen) → 3+0+1 = 4
    val x0 = v(0); val x1 = v(1)
    val s = And(List(x0, x1))
    assertEquals(cs(Or(List(s, Neg(s)))), BigInt(4))
  }

  test("sharing: three-way diamond DAG") {
    // x is a leaf shared by two inner nodes which are both used by a root
    // root = And([Or([x,y]), Or([x,z])])
    //   first Or([x,y]):  x→1, y→1, +1 = 3
    //   second Or([x,z]): x already seen → 0, z→1, +1 = 2
    //   And: 3+2+1 = 6
    val x = v(0); val y = v(1); val z = v(2)
    val left  = Or(List(x, y))
    val right = Or(List(x, z))
    val root  = And(List(left, right))
    assertEquals(cs(root), BigInt(6))
  }

  test("sharing: 4-ary And with two shared pairs") {
    // And([s1, s2, s1, s2]) where s1 = Or([x0,x1]), s2 = Or([x2,x3])
    // s1 first: 3; s2 first: 3; s1 again: 0; s2 again: 0; +(4-1) = 3 → total = 9
    val s1 = Or(List(v(0), v(1)))
    val s2 = Or(List(v(2), v(3)))
    assertEquals(cs(And(List(s1, s2, s1, s2))), BigInt(9))
  }

  // ── Multiple roots sharing a subformula ────────────────────────────────────

  test("multi-root: circuitSize counts shared subformula once across roots") {
    // circuitSize([s, s]) where s = And([x0,x1])
    // foldedSize(s) = 3; foldedSize(s) = 0 (seen) → total = 3
    val x0 = v(0); val x1 = v(1)
    val s = And(List(x0, x1))
    assertEquals(cs(List(s, s)), BigInt(3))
  }

  test("multi-root: two independent formulas cost is the sum") {
    // circuitSize([And([x0,x1]), And([x2,x3])]) — distinct objects
    val f1 = And(List(v(0), v(1)))
    val f2 = And(List(v(2), v(3)))
    assertEquals(cs(List(f1, f2)), BigInt(3) + BigInt(3))
  }

  test("multi-root: shared leaf across two roots") {
    // circuitSize([And([x, y]), Or([x, z])]) — x is the same object
    val x = v(0); val y = v(1); val z = v(2)
    val f1 = And(List(x, y))
    val f2 = Or(List(x, z))
    // f1: x→1, y→1, +1 = 3; f2: x→0(seen), z→1, +1 = 2; total = 5
    assertEquals(cs(List(f1, f2)), BigInt(5))
  }

  // ── Distinct objects with same variable id are NOT shared ─────────────────

  test("two distinct Variable(0) objects are NOT shared — cost adds up") {
    val x1 = Variable(0)
    val x2 = Variable(0)   // different object, different uniqueKey
    // Or([x1, x2]): x1→1, x2→1 (different keys), +1 = 3
    assertEquals(cs(Or(List(x1, x2))), BigInt(3))
  }

  // ── Edge cases ────────────────────────────────────────────────────────────

  test("empty list has circuit size 0") {
    assertEquals(cs(List.empty[Formula]), BigInt(0))
  }

  test("single-child And([x]) costs 1 + (1-1) = 1") {
    assertEquals(cs(And(List(v(0)))), BigInt(1))
  }

  test("single-child Or([x]) costs 1") {
    assertEquals(cs(Or(List(v(0)))), BigInt(1))
  }
}
