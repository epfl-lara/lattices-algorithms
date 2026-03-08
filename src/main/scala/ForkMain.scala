package ortholattices
import ortholattices.algorithms.*
import Datastructures.*
import Benchmark.{Algo, Improvement}
import scala.concurrent.duration.*

object ForkMain {

  def main(args: Array[String]): Unit = args match {
    case Array("sizes", folder) =>
      val cases = List("adder","bar","div","hyp","log2","max","multiplier","sin","sqrt","square")
      cases.foreach { c =>
        val path = s"$folder/$c.aig"
        val formulas = AigerParser.getAigerFormulas(path)
        println(s"$c: ${circuitSize(formulas)}")
      }
    case Array("epfl", folder) =>
      Benchmark.epflAigerBenchmark(folder + "/", 5.minutes)
    case Array("epfl", folder, timeoutMin) =>
      Benchmark.epflAigerBenchmark(folder + "/", timeoutMin.toLong.minutes)
    case Array(algoStr, path) =>
      val algo = algoStr match {
        case "ocbsl" => Algo.OCBSL
        case "ol"         => Algo.OL
        case "structural" => Algo.STRUCTURAL
        case _ => println(s"Unknown algo: $algoStr"); sys.exit(1)
      }
      val formulas = AigerParser.getAigerFormulas(path)
      val t0 = System.nanoTime()
      val value = benchmark(algo, formulas)
      val elapsedMs = (System.nanoTime() - t0) / 1_000_000
      println(f"$algo algorithm succeeded with an average improvement ratio of ${value.ratio}%1.4f.")
      println(s"RESULT ${value.reduced} $elapsedMs")
    case _ =>
      println("Usage: ForkMain <ol|ocbsl|structural> <path>  |  ForkMain epfl <folder> [timeoutMinutes]")
      sys.exit(1)
  }

  def benchmark(algo: Algo, formulas: List[Formula]): Improvement = algo match {
    case Algo.OCBSL      => benchmarkOcbsl(formulas)
    case Algo.OL         => benchmarkOl(formulas)
    case Algo.STRUCTURAL => benchmarkStructural(formulas)
  }

  def benchmarkOcbsl(formulas: List[Formula]): Improvement = {
    val algo = new OcbslAlgorithm
    val total: (BigInt, BigInt) = (circuitSize(formulas), circuitSize(formulas map algo.reducedForm))
    Improvement(total._1, total._2, (BigDecimal(total._2) / BigDecimal(total._1)).toDouble)
  }

  def benchmarkOl(formulas: List[Formula]): Improvement = {
    val algo = new OLAlgorithm
    val total: (BigInt, BigInt) = (circuitSize(formulas), circuitSize(formulas map algo.reducedForm))
    Improvement(total._1, total._2, (BigDecimal(total._2) / BigDecimal(total._1)).toDouble)
  }

  def benchmarkStructural(formulas: List[Formula]): Improvement = {
    val algo = new OLAlgorithmStructural
    val total: (BigInt, BigInt) = (circuitSize(formulas), circuitSize(formulas map algo.reducedForm))
    Improvement(total._1, total._2, (BigDecimal(total._2) / BigDecimal(total._1)).toDouble)
  }
}
