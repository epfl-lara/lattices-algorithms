package ortholattices
import ortholattices.algorithms.Datastructures.*
import FormulaGenerator.*
import Benchmark.*
import FormulaGenerator.*

import scala.concurrent.duration.*

object Main {
  var testing = true
  def main(args: Array[String]): Unit = {
    val positional = args.filter(_(0) != '-')
    val options = args.filter(_(0) == '-')

    val number:Int = positional(0).toInt
    val size: Int = positional(1).toInt
    val vars: Int = positional(2).toInt
    val check: Boolean = options.contains("-check")
    val ocbsl: Boolean = !options.contains("-nocbsl")
    val ol: Boolean = !options.contains("-nol")

    val folder = ((s:String) => if s.last == '/' then s else s+'/')(positional(3))
    val cases = positional(4).split(Array(' ', ','))

    main(number, size, vars, check, folder, cases, ocbsl, ol)


  }
  def main(number: Int, size: Int, vars: Int, check: Boolean, folder: String, cases: Array[String], ocbsl:Boolean=true, ol:Boolean=true): Unit = {
    if (number > 0) {
      /*
       * Produces random formulas and print their size and the size of their reduced form.
       * if check is true, verify that the reduced formulas are logically equivalent
       * (in propositional logic) to the original formula.
       */
      saveRandomBenchmark(number, size, vars, check, folder, ocbsl, ol)
    }
    if (cases.nonEmpty){
      /**
       * Parse a set of circuits in Aiger format coming from hardware.
       * Those benchmark are intended to compare optimization methods and-
       * are pre-optimized to various degrees.
       * https://www.epfl.ch/labs/lsi/page-102566-en-html/benchmarks/
       */
      epflAigerBenchmark(folder, cases, 3.seconds)
    }
  }



}
