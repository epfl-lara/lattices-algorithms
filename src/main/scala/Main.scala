import algorithms.Datastructures.*
import FormulaGenerator.*
import Benchmark.*
object Main {
  var testing = true
  def main(args: Array[String]): Unit = {
    val folder = "/home/sguillou/Desktop/aiger/"

    /*
     * Produces random formulas and print their size and the size of their reduced form.
     * if check is true, verify that the reduced formulas are logically equivalent
     * (in propositional logic) to the original formula.
     */
    printRandomBenchmark(10, 10000, 50, false)

    /**
     * Parse a set of circuits in Aiger format coming from hardware.
     * Those benchmark are intended to compare optimization methods and-
     * are pre-optimized to various degrees.
     * https://www.epfl.ch/labs/lsi/page-102566-en-html/benchmarks/
     */
    epflAigerBenchmark(folder, 5)


  }




  val a = Variable(0)
  val b = Variable(1)
  val c = Variable(2)
  val d = Variable(3)
  val e = Variable(4)
  val f = Variable(5)

  val x0 = Variable(0)
  val x1 = Variable(1)
  val x2 = Variable(2)
  val x3 = Variable(3)
  val x4 = Variable(4)
  val x5 = Variable(5)


  def neg(f: Formula): Formula = Neg(f)

  def and(args: List[Formula]): Formula = And(args)

  def and(f:Formula*): Formula = And(f.toList)

  def or(args: List[Formula]): Formula = Or(args)

  def or(f:Formula*): Formula = Or(f.toList)

  def iff(f: Formula, g: Formula): Formula = and(implies(f, g), implies(g, f))

  def implies(f: Formula, g: Formula): Formula = neg(or(neg(f), g))


}
