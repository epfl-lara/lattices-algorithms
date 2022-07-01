import Datastructures.*
import FormulaGenerator.*

import java.util.concurrent.TimeoutException
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.*
import scala.concurrent.duration.*
import scala.util.{Failure, Success, Try}


object Benchmark {


  def epflAigerBenchmark(folder:String, cases:List[String]):Unit = {
    cases.foreach(c =>
      println(s"Benchmark $c:")
      aigerBenchmark(folder+c)
    )
  }
  def epflAigerBenchmark(folder:String):Unit = {
    val cases = List("adder.aig", "bar.aig", "div.aig", "hyp.aig",
      "log2.aig", "max.aig", "multiplier.aig", "sin.aig", "sqrt.aig", "square.aig")
    epflAigerBenchmark(folder, cases)
  }


  def aigerBenchmark(path:String): Unit = {
    val timeout = 60
    val formulas = AigerParser.getAigerFormulas(path)
    val rOcbsl =  runWithTimeout(timeout*1000)(benchmarkOcbsl(formulas))
    //val rOl =  runWithTimeout(timeout*1000)(benchmarkOl(formulas))
    rOcbsl match
      case Success(value) =>
        println(f"OCBSL algorithm succeeded with an average improvement" +
          f" ratio of ${value.ratio}%1.4f (original formula of size ${bigIntRepr(value.original)})")
      case Failure(exception) => println(f"OCBSL algorithm didn't succeed in ${timeout}s (${exception.getClass})" +
        f" (original formula of size ${bigIntRepr(formulas.map(_.size).sum)})")
/*
    rOl match
      case Success(value) =>
        println(f"OL algorithm succeeded with an average improvement" +
          f" ratio of ${value.ratio}%1.4f (original formula of size ${bigIntRepr(value.original)})")
      case Failure(exception) => println(f"OL algorithm didn't succeed in ${timeout}s (${exception.getClass})" +
        f" (original formula of size ${bigIntRepr(formulas.map(_.size).sum)})")
*/
  }

  def benchmarkOcbsl(formulas:List[Formula]) : Improvement = {
    val algo = new OcbslAlgorithm
    val mean:(BigInt, BigInt) = formulas.foldLeft((0:BigInt, 0:BigInt)){
      case ((a,b), f) =>
        val r = algo.reducedForm(f)
        (a+f.size,b+r.size)}
    Improvement(mean._1, mean._2, (BigDecimal(mean._1)/BigDecimal(mean._2)).toDouble)
  }

  def benchmarkOl(formulas:List[Formula]) : Improvement = {
    val algo = new OLAlgorithm
    val mean:(BigInt, BigInt) = formulas.foldLeft((0:BigInt, 0:BigInt)){
      case ((a,b), f) =>
        val r = algo.reducedForm(f)
        (a+f.size,b+r.size)}
    Improvement(mean._1, mean._2, (BigDecimal(mean._1)/BigDecimal(mean._2)).toDouble)
  }

  def bigIntRepr(n:BigInt) :String = {
    if n < 1000000 then s"$n" else f"env. 10^${n.toString.length}"
  }

  case class Improvement(original:BigInt, reduced:BigInt, ratio:Double)

  def checkResult(r: Result, n: Int): Unit = {
    val check1 = checkEquivalence(r.originalFormula, r.ocbslFormula, n)
    val check2 = checkEquivalence(r.originalFormula, r.olFormula, n)
    if !check1._1 then println(s"    check for OCBSL failed on res ${check1._2}<------------------------------------------------")
    if !check2._1 then println(s"    check for OL failed on res ${check2._2} <------------------------------------------------")
    if check1._1 && check2._1 then println("    checks are correct")
    else println(s"    ${Printer.prettyFull(r.originalFormula)}")
  }

  def printResult(r: Result): Unit = {
    println(s"Original formula ${r.originalFormula} of size ${r.originalSize}")
    println(s"    OCBSL formula ${r.ocbslFormula} of size ${r.resultingSizeOCBSL}")
    println(s"    OL    formula ${r.olFormula} of size ${r.resultingSizeOL}")
  }
  def sparsePrintResult(r: Result): Unit = {
    println(s"Original formula of size ${bigIntRepr(r.originalSize)}")
    println(f"    OCBSL formula of size ${bigIntRepr(r.resultingSizeOCBSL)}" +
      f" (ratio ${BigDecimal(r.resultingSizeOCBSL)/BigDecimal(r.originalSize)}%1.5f )")
    println(f"    OL    formula of size ${bigIntRepr(r.resultingSizeOL)} (ratio ${BigDecimal(r.resultingSizeOL.toDouble)/BigDecimal(r.originalSize)}%1.5f )")

  }




  def makeResult(f: Formula, algos:Option[(OcbslAlgorithm, OLAlgorithm)]=None): Result = {
    algos match
      case Some(value) =>
        val r1 = value._1.reducedForm(f)
        val r2 = value._2.reducedForm(f)
        //Result(circuitSize(f), circuitSize(r1), circuitSize(r2), f, r1, r2)
        Result(f.size, r1.size, r2.size, f, r1, r2)
      case None =>
        val r1 = OcbslAlgorithm.reducedForm(f)
        val r2 = OLAlgorithm.reducedForm(f)
        //Result(circuitSize(f), circuitSize(r1), circuitSize(r1), f, r1, r1)
        Result(f.size, r1.size, r2.size, f, r1, r2)
  }

  case class Result(originalSize: BigInt, resultingSizeOCBSL: BigInt, resultingSizeOL: BigInt, originalFormula: Formula, ocbslFormula: Formula, olFormula: Formula)

  def benchmark(number: Int, size: Int, variables: Int): List[Result] = {
    if number <= 0 then Nil
    else {
      val r = randomFormula(size, variables)
      makeResult(r) :: benchmark(number - 1, size, variables)
    }
  }


  def runWithTimeout[T](timeoutMs: Long)(f: => T) : Try[T] = {
    val r =
      try{Await.ready(Future(try{Success(f)} catch {case t:Throwable => Failure(t)}), timeoutMs.milliseconds).value}
      catch {case e: Throwable => Some(Failure(e))}
    r match
      case Some(value) => value.flatten
      case None => Failure(new TimeoutException)
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
