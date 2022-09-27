import algorithms.Datastructures.*
import FormulaGenerator.*
import algorithms.{OLAlgorithm, OcbslAlgorithm, Printer}

import java.util.concurrent.TimeoutException
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.*
import scala.concurrent.duration.*
import scala.util.{Failure, Success, Try}
import java.io._
import java.text.SimpleDateFormat;
import java.util.Date;

object Benchmark {

  def epflAigerBenchmark(folder:String):Unit = {
    val cases = Array("adder.aig", "bar.aig", "div.aig", "hyp.aig",
      "log2.aig", "max.aig", "multiplier.aig", "sin.aig", "sqrt.aig", "square.aig")
    epflAigerBenchmark(folder, cases)
  }
  def epflAigerBenchmark(folder:String, cases:Array[String]):Unit = {
    cases.foreach { c =>
      aigerBenchmark(folder + c)
    }
  }

  def aigerBenchmark(path:String): Unit = {

    val name = path.split('/').last
    val folder = path.split('/').dropRight(1).mkString("/")+"/"
    val fileObject = new File(folder+s"results/$name.txt" )
    val printWriter = new PrintWriter(fileObject)
    val date = SimpleDateFormat("dd/MM/yyyy HH:mm:ss").format(Date())
    printWriter.write(s"Banchmark $name, $date\n")
    printWriter.flush()
    println("datetime")
    val formulas = AigerParser.getAigerFormulas(path)
    printWriter.write(s"Original circuit of size ${bigIntRepr(formulas.map(_.circuitSize).sum)}\n")
    printWriter.flush()

    val rOcbsl = Try(benchmarkOcbsl(formulas))
    rOcbsl match
      case Success(value) =>
        printWriter.write(f"OCBSL algorithm succeeded with an average improvement ratio of ${value.ratio}%1.4f.\n")
      case Failure(exception) =>
        printWriter.write(f"OCBSL algorithm didn't succeed and raised ${exception.getClass}.\n")
    printWriter.flush()

    val rOl = Try(benchmarkOl(formulas))
    rOl match
      case Success(value) =>
        printWriter.write(f"OL algorithm succeeded with an average improvement ratio of ${value.ratio}%1.4f.\n")
      case Failure(exception) =>
        printWriter.write(f"OL algorithm didn't succeed and raised(${exception.getClass}.\n")

    printWriter.write("SUCCESS\n")
    printWriter.close()
  }

  case class Improvement(original:BigInt, reduced:BigInt, ratio:Double)
  case class Result(originalSize: BigInt, resultingSizeOCBSL: BigInt, resultingSizeOL: BigInt, originalFormula: Formula, ocbslFormula: Formula, olFormula: Formula)


  def benchmarkOcbsl(formulas:List[Formula]) : Improvement = {
    val algo = new OcbslAlgorithm
    val total:(BigInt, BigInt) = (circuitSize(formulas), circuitSize(formulas map algo.reducedForm))
    Improvement(total._1, total._2, (BigDecimal(total._2)/BigDecimal(total._1)).toDouble)
  }

  def benchmarkOl(formulas:List[Formula]) : Improvement = {
    val algo = new OLAlgorithm
    val total:(BigInt, BigInt) = (circuitSize(formulas), circuitSize(formulas map algo.reducedForm))
    Improvement(total._1, total._2, (BigDecimal(total._2)/BigDecimal(total._1)).toDouble)
  }

  def bigIntRepr(n:BigInt) :String = {
    if n < 1000000 then s"$n" else f"env. 10^${n.toString.length}"
  }

  def checkResult(r: Result, n: Int, write: String => Unit = println): Unit = {
    val check1 = checkEquivalence(r.originalFormula, r.ocbslFormula, n)
    val check2 = checkEquivalence(r.originalFormula, r.olFormula, n)
    if !check1._1 then write(s"    check for OCBSL failed on assignement ${check1._2}<--------------------------------------------")
    if !check2._1 then write(s"    check for OL failed on assignement ${check2._2} <--------------------------------------------")
    if check1._1 && check2._1 then write("    checks are correct")
    else println(s"    ${Printer.prettyFull(r.originalFormula)}")
  }

  def sparseSaveResult(r: Result, write:String => Unit, ocbsl:Boolean = true, ol:Boolean = true): Unit = {
    write(s"Original formula of size ${bigIntRepr(r.originalSize)}\n")

    if ocbsl then write(f"    OCBSL formula of size ${bigIntRepr(r.resultingSizeOCBSL)}" +
      f" (ratio ${BigDecimal(r.resultingSizeOCBSL)/BigDecimal(r.originalSize)}%1.5f )\n")
    if ol then write(f"    OL    formula of size ${bigIntRepr(r.resultingSizeOL)} (ratio ${BigDecimal(r.resultingSizeOL.toDouble)/BigDecimal(r.originalSize)}%1.5f )\n")

  }


  /**
   * Compute the circuit size of a formula and of its reduced forms
   */
  def makeResult(f: Formula, ocbsl:Boolean, ol:Boolean, algos:Option[(OcbslAlgorithm, OLAlgorithm)]=None, circuit:Boolean = true): Result = {
    val (a1: EquivalenceAndNormalFormAlgorithm, a2:EquivalenceAndNormalFormAlgorithm) = algos match
      case Some(value) => (value._1, value._2)
      case None => (OcbslAlgorithm, OLAlgorithm)
    val r1 = if ocbsl then a1.reducedForm(f) else f
    val r2 = if ol then a2.reducedForm(f) else f
    if circuit then
      Result(f.circuitSize, r1.circuitSize, r2.circuitSize, f, r1, r2)
    else
      Result(f.size, r1.size, r2.size, f, r1, r2)
  }

  /**
   * Produces random formulas and print their size and the size of their reduced form.
   * if check is true, verify that the reduced formulas are logically equivalent
   * (in propositional logic) to the original formula.
   */
  def saveRandomBenchmark(number:Int, size:Int, variables:Int, check:Boolean, path:String, ocbsl:Boolean=true, ol:Boolean= true): Unit = {
    val fileObject = new File(path+"/results/random.txt" )
    val printWriter = new PrintWriter(fileObject)

    val date = SimpleDateFormat("dd/MM/yyyy HH:mm:ss").format(Date())
    printWriter.write(s"Random Benchmark, $date\n")
    printWriter.write(s"Parameters: $number formulas, size max $size, $variables different variables, check:$check, ocbsl:$ocbsl, ol:$ol\n")

    var totgen:Long = 0
    var toteval:Long = 0

    var rs:List[Result] = Nil
    (1 to number) foreach { _ =>
      val (r, tgen, teval) = benchmark(size, variables, ocbsl, ol)
      totgen+=tgen
      toteval+=teval
      rs = r::rs
      if check then checkResult(r, variables, printWriter.write)
      sparseSaveResult(r, printWriter.write, ocbsl, ol)
      printWriter.flush()
    }
    val ocbslMean = rs.map(r => BigDecimal(r.resultingSizeOCBSL.toDouble)/BigDecimal(r.originalSize)).sum/rs.size
    val olMean = rs.map(r => BigDecimal(r.resultingSizeOL.toDouble)/BigDecimal(r.originalSize)).sum/rs.size
    printWriter.write(s"Average OCBSL improvement: $ocbslMean\n")
    printWriter.write(s"Average OL improvement: $olMean\n")
    printWriter.write(s"Total timegenerating: ${totgen/1000000}ms, average time generating:${totgen/(1000000*number)} ms.\n")
    printWriter.write(s"Total time: ${toteval/1000000}ms, average time:${toteval/(1000000*number)} ms.\n")
    printWriter.close()
  }

  def benchmark(size: Int, variables: Int, ocbsl:Boolean, ol:Boolean): (Result, Long, Long) = {
    val t0 = System.nanoTime()
    val r = randomFormula(size, variables)
    val t1 = System.nanoTime()
    val re = makeResult(r, ocbsl, ol, circuit = false)
    val t2 = System.nanoTime()
    (re, t1-t0, t2-t1)

  }

  //
  //
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  //                                                                                                                                               //
  //   Further is the evaluation function for testing correctness. Don't read that code before creating one such function yourself independently!  //
  //                                                                                                                                               //
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  //
  //
  //
  //
  //
  //
  //
  //
  //
  //
  //
  //
  //
  //
  //
  //
  //
  //
  def checkEquivalence(f1: Formula, f2: Formula, n: Int): (Boolean, String) = {
    var seed = 0
    val bound = scala.math.pow(2, n)
    while (seed < bound) {
      val assi = createAssignement(seed, n)
      if !(eval(f1, assi) == eval(f2, assi)) then return (false, seed.toBinaryString)
      seed += 1

    }
    (true, seed.toBinaryString)
  }

  def createAssignement(seed: Int, length: Int): Int => Boolean = {
    val s = seed.toBinaryString.reverse.padTo(length, '0').reverse
    v => s(v) == '1'
  }

  def eval(f: Formula, assignment: Int => Boolean): Boolean = f match
    case Variable(id) => assignment(id)
    case Neg(child) => !eval(child, assignment)
    case Or(children) => children.exists(eval(_, assignment))
    case And(children) => children.forall(eval(_, assignment))
    case Literal(b) => b
}
