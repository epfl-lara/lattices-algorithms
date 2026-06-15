package ortholattices
import ortholattices.algorithms.Datastructures.*

import scala.util.Random

object FormulaGenerator {

  /**
   * Generates a hard non-clausal random formula following the fixed-shape model of
   * Navarro & Voronkov, "Generation of Hard Non-Clausal Random Satisfiability Problems",
   * AAAI 2005.
   *
   * The shape <k1,...,kd> defines a balanced AND/OR tree whose root is OR:
   *   - <k1,...,kd>  = OR  of k1 instances of [k2,...,kd]
   *   - [k1,...,kd]  = AND of k1 instances of <k2,...,kd>
   *   - <>  /  []    = a literal (variable or its negation)
   *
   * The returned formula is the conjunction of `m` independent random instances of
   * the disjunctive shape <shape>, over variables x0..x(n-1).
   * As in the paper, m = round(r * n).
   *
   * Example: shape = List(2,2,2), n = 10, r = 4.0 produces an AND of 40 instances
   * of the shape (OR(AND(OR(lit,lit),OR(lit,lit)), AND(OR(lit,lit),OR(lit,lit)))).
   *
   * @param shape  the arity sequence [k1, k2, ..., kd]; must be non-empty and all ki >= 2
   * @param n      number of distinct variables
   * @param r      ratio of instances to variables; m = round(r * n)
   * @param seed   optional RNG seed for reproducibility
   */
  def hardFormula(shape: List[Int], n: Int, r: Double, seed: Option[Int] = None): Formula = {
    require(n >= 1, "n must be >= 1")
    require(shape.forall(_ >= 2), "all shape arities must be >= 2")
    val m = math.round(r * n).toInt max 1
    val rng: Random = seed.fold(new Random())(new Random(_))
    val variables = Array.tabulate(n)(Variable(_))

    def randomLiteral(): Formula =
      val v = variables(rng.nextInt(n))
      if rng.nextBoolean() then v else Neg(v)

    // isDisjunctive=true  =>  produce a <ks>-instance  (OR at root)
    // isDisjunctive=false =>  produce a [ks]-instance  (AND at root)
    def instance(ks: List[Int], isDisjunctive: Boolean): Formula = ks match
      case Nil     => randomLiteral()
      case k :: rest =>
        val children = List.fill(k)(instance(rest, !isDisjunctive))
        if isDisjunctive then Or(children) else And(children)

    val instances = List.fill(m)(instance(shape, isDisjunctive = true))
    instances match
      case List(single) => single
      case many         => And(many)
  }

  /**
   * Generates a single deep formula (m=1) with hierarchical variable locality.
   *
   * Variables are allocated in blocks: all leaves within the same subtree of height
   * `localDepth` share a pool of `poolPerGroup` variables.  Different subtrees at
   * that height get completely independent pools.
   *
   * For an all-binary shape of depth d (shape = [2]*d):
   *   n  = 2^(d − localDepth) × poolPerGroup   (linear in size 2^d when l,p are constants)
   *   avg occurrences per var = 2^localDepth / poolPerGroup   (all within a local subtree)
   *
   * Edge cases:
   *   localDepth >= d  →  one global pool: n = poolPerGroup, max reuse
   *   localDepth = 0   →  each leaf has its own pool: n = leaves × poolPerGroup, no reuse
   *
   * @param shape        arity sequence [k1,...,kd]; must be non-empty
   * @param localDepth   height (in levels from leaves) of each local variable region
   * @param poolPerGroup number of distinct variables per local region
   * @param seed         optional RNG seed
   * @return             (formula, nVars) where nVars is the total number of distinct variables
   */
  def hardFormulaLocal(
      shape: List[Int],
      localDepth: Int,
      poolPerGroup: Int,
      seed: Option[Int] = None
  ): (Formula, Int) = {
    require(shape.forall(_ >= 2), "all shape arities must be >= 2")
    require(localDepth >= 0, "localDepth must be >= 0")
    require(poolPerGroup >= 1, "poolPerGroup must be >= 1")

    val d   = shape.length
    val rng = seed.fold(new Random())(new Random(_))
    var nextVarIdx = 0

    def allocatePool(): Array[Formula] =
      val pool: Array[Formula] = Array.tabulate(poolPerGroup)(i => Variable(nextVarIdx + i))
      nextVarIdx += poolPerGroup
      pool

    // ks.length = levels remaining (d at root, 0 at leaf-parent)
    // vars = variable pool for the current local region
    //
    // When ks.length == localDepth + 1: we are at the exact level that spawns local regions.
    //   → allocate a fresh independent pool for EACH child.
    // When ks.length < localDepth + 1 (within a region) or > localDepth + 1 (above, passing through):
    //   → all children share the same vars.
    def instance(ks: List[Int], isDisjunctive: Boolean, vars: Array[Formula]): Formula = ks match
      case Nil =>
        val v = vars(rng.nextInt(vars.length))
        if rng.nextBoolean() then v else Neg(v)
      case k :: rest =>
        val children =
          if ks.length == localDepth + 1 then
            // Spawn a new local region for each child subtree
            List.tabulate(k)(_ => instance(rest, !isDisjunctive, allocatePool()))
          else
            List.fill(k)(instance(rest, !isDisjunctive, vars))
        if isDisjunctive then Or(children) else And(children)

    // If the whole tree fits inside a single local region, allocate one pool at the root.
    // Otherwise start with an empty sentinel (will never be read before the boundary is reached).
    val initialVars = if localDepth >= d then allocatePool() else Array.empty[Formula]
    val formula     = instance(shape, isDisjunctive = true, initialVars)
    (formula, nextVarIdx)
  }

  def randomFormula(size: Int, n: Int, seed: Option[Int] = None, doFlatten: Boolean = true): Formula = {
    val variables = List.range(0, n).map(i => Variable(i))
    val rg: Random = seed match
      case Some(value) => new Random(value)
      case None => new Random()
    def single(size: Int): Formula = {
      if size <= 1 then {
        val v = variables(rg.nextInt(variables.size))
        if rg.nextBoolean() then v else Neg(v)
      } else
        val split = rg.nextInt(size - 1)
        val i1 = single(split+1)
        val i2 = single(size - split-2)
        val f = if rg.nextBoolean() then Or(List(i1, i2)) else And(List(i1, i2))
        f
    }
    val nnf = negationNormalForm(single(size))
    if doFlatten then flatten(nnf) else nnf
  }

}
