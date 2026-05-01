package ortholattices

import ortholattices.algorithms.Datastructures.*

import scala.util.Random

/**
 * Generates random circuits (DAGs with structure sharing).
 *
 * Unlike FormulaGenerator which produces binary trees, this produces formulas
 * where sub-expressions are reused multiple times.  Each gate in the circuit
 * references one or more earlier gates, building a proper DAG.
 *
 * @param numGates number of internal gate nodes (And / Or / Neg) to create
 * @param numVars  number of input variables (0 … numVars-1)
 * @param seed     optional RNG seed for reproducibility
 * @return the last gate node as the circuit output (a DAG with sharing)
 */
object CircuitGenerator {

  def randomCircuit(numGates: Int, numVars: Int, seed: Option[Int] = None): Formula = {
    val rg: Random = seed.map(new Random(_)).getOrElse(new Random())

    // Pool of available nodes; new gates pick their operands from this pool.
    // Starting with variables (and literal true/false at low probability).
    val pool = scala.collection.mutable.ArrayBuffer[Formula]()
    (0 until numVars).foreach(i => pool += Variable(i))

    // Ensure at least one node in pool before building gates
    if pool.isEmpty then pool += Literal(false)

    for (_ <- 0 until numGates) {
      val gate: Formula =
        if pool.size == 1 then
          Neg(pool(0))
        else
          val op = rg.nextInt(3)
          op match
            case 0 => // Neg: invert one operand
              Neg(pool(rg.nextInt(pool.size)))
            case 1 => // And: 2–4 operands, picked with repetition (enabling sharing)
              val arity = 2 + rg.nextInt(3)
              And(List.fill(arity)(pool(rg.nextInt(pool.size))))
            case _ => // Or: same structure
              val arity = 2 + rg.nextInt(3)
              Or(List.fill(arity)(pool(rg.nextInt(pool.size))))
      pool += gate
    }

    pool.last
  }

  /**
   * Returns a circuit with guaranteed heavy structure sharing.
   *
   * Strategy:
   *  1. Build a small "shared" layer of intermediate nodes over the base
   *     variables only — these become the heavily-reused hot nodes.
   *  2. Build a larger "consumer" layer where each gate picks its operands
   *     with a 60% bias towards the shared nodes (high fan-out) but can
   *     also pick from any earlier gate (enabling proper DAG chaining).
   *  3. Return pool.last as the circuit output.  This keeps the full DAG
   *     structure intact and avoids the massive collapse caused by wrapping
   *     everything in a top-level And.
   *
   * @param numGates total number of gate nodes (shared + consumer layers)
   * @param numVars  number of input variables
   * @param seed     optional RNG seed for reproducibility
   */
  def highSharingCircuit(numGates: Int, numVars: Int, seed: Option[Int] = None): Formula = {
    val rg: Random = seed.map(new Random(_)).getOrElse(new Random())

    val pool = scala.collection.mutable.ArrayBuffer[Formula]()
    (0 until numVars).foreach(i => pool += Variable(i))
    if pool.isEmpty then pool += Literal(false)

    val baseSize = pool.size

    // Build shared layer: gates over base variables only.
    // Capped at 25% of numGates so most gates become consumers.
    val numShared = math.max(1, math.min(numGates / 4, 8))

    for (_ <- 0 until numShared) {
      val g =
        if baseSize == 1 then Neg(pool(0))
        else
          val pickVar = () => pool(rg.nextInt(baseSize))
          rg.nextInt(3) match
            case 0 => Neg(pickVar())
            case 1 => And(List.fill(2 + rg.nextInt(2))(pickVar()))
            case _ => Or(List.fill(2 + rg.nextInt(2))(pickVar()))
      pool += g
    }

    val sharedEnd = pool.size // shared nodes live in pool[baseSize..sharedEnd)

    // Build consumer layer.
    // Each operand is picked from the shared pool (60%) or any earlier gate (40%).
    // The 40% full-pool picks enable chains: later gates can reference earlier consumers,
    // creating multi-level DAG paths through the shared backbone.
    for (_ <- 0 until (numGates - numShared)) {
      def pick(): Formula =
        if rg.nextDouble() < 0.6 && sharedEnd > baseSize then
          pool(baseSize + rg.nextInt(sharedEnd - baseSize)) // biased: a shared node
        else
          pool(rg.nextInt(pool.size))                       // any earlier node
      val g =
        if pool.size == 1 then Neg(pool(0))
        else rg.nextInt(3) match
          case 0 => Neg(pick())
          case 1 => And(List.fill(2 + rg.nextInt(2))(pick()))
          case _ => Or(List.fill(2 + rg.nextInt(2))(pick()))
      pool += g
    }

    // Return the last gate.  It sits at the top of a proper DAG whose
    // intermediate nodes are shared across multiple paths.
    pool.last
  }
}
