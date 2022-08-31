import Datastructures.*
import FormulaGenerator.*
import Benchmark.*
object Main {
  var testing = true
  def main(args: Array[String]): Unit = {
    val folder = "/home/sguillou/Desktop/aiger/"
    epflAigerBenchmark(folder)
/*
    val adderFormulas = AigerParser.getAigerFormulas(folder+"div.aig")
    //val algos = Some((new OcbslAlgorithm, new OLAlgorithm))
    //adderFormulas.slice(adderFormulas.length - 1, adderFormulas.length - 1 + 1).foreach { f =>
    adderFormulas.foreach { f =>

      println(s"formula size: ${bigIntRepr(circuitSize(f))}")
      println(s"formula depth: ${bigIntRepr(f.depth)}")

      val r = makeResult(f)
      sparsePrintResult(r)

      /*
      println(s"original formula size: ${r.originalFormula.size}")
      println(s"ol formula size: ${r.olFormula.size}")
      println(s"ocbsl formula size: ${r.ocbslFormula.size}")
      println(s"original formula circuit size: ${r.originalFormula.circuitSize}")
      println(s"ol formula circuit size: ${r.olFormula.circuitSize}")
      println(s"ocbsl formula circuit size: ${r.ocbslFormula.circuitSize}")
      println(s"formula original: ${r.originalFormula}")
      println(s"formula ocbsl   : ${r.ocbslFormula}")
      println(s"formula ol      : ${r.olFormula}")
      val pol = OLAlgorithm.polarize(r.originalFormula)
      println(s"formula pol     : ${pol}")
      val npf = OLAlgorithm.nPnormalForm(pol)
      println(s"formula npf     : ${npf}")
      println(s"formula ret     : ${OLAlgorithm.toFormula(npf)}")
      */

    }
*/
/*
    val rs = benchmark(10, 300, 10)
    rs.foreach { r =>
      checkResult(r, 10)
      sparsePrintResult(r)
    }
*/
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
