import Datastructures.*
import FormulaGenerator.*
object Main {

  def main(args: Array[String]): Unit = {
   val folder = "/home/sguillou/Desktop/aiger/"


    val adderFormulas = AigerParser.getAigerFormulas(folder+"multiplier.aig")
    println(totalNumberFormula)
    val algos = Some((new OcbslAlgorithm, new OLAlgorithm))

    adderFormulas.foreach { f =>
      val r = makeResult(f, algos)
      sparsePrintResult(r)
    }

    /*
    println("before:Adder, after:bench")
    val rs = benchmark(10, 1000, 50)
    rs.foreach { r =>
      sparsePrintResult(r)
    }
    */

  }

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
    println(s"    OL formula ${r.olFormula} of size ${r.resultingSizeOL}")
  }
  def sparsePrintResult(r: Result): Unit = {
    println(s"Original formula of size ${r.originalSize}")
    println(f"    OCBSL formula of size ${r.resultingSizeOCBSL}" +
      f" (ratio ${BigDecimal(r.resultingSizeOCBSL)/BigDecimal(r.originalSize)}%1.10f )")
    //println(f"    OL formula of size ${r.resultingSizeOL} (ratio ${BigDecimal(r.resultingSizeOL.toDouble)/BigDecimal(r.originalSize)}%1.8f )")

  }




  def makeResult(f: Formula, algos:Option[(OcbslAlgorithm, OLAlgorithm)]=None): Result = {
    algos match
      case Some(value) =>
        val r1 = value._1.reducedForm(f)
        //val r2 = value._2.reducedForm(f)
        Result(f.size, r1.size, r1.size, f, r1, r1)
      case None =>
        val r1 = OcbslAlgorithm.reducedForm(f)
        val r2 = OLAlgorithm.reducedForm(f)
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
