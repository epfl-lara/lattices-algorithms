import algorithms.Datastructures.*
import algorithms.{OLAlgorithm, OcbslAlgorithm, Printer}
import Benchmark.{Algo, Improvement}

object ForkMain {

  def main(args: Array[String]): Unit = {
    val (algo, path) = args match {
      case Array("ocbsl", path) => (Algo.OCBSL, path)
      case Array("ol", path) => (Algo.OL, path)
      case _ =>
        println("Unknown options")
        sys.exit(1)
    }

    val formulas = AigerParser.getAigerFormulas(path)
    val value = benchmark(algo, formulas)
    println(f"$algo algorithm succeeded with an average improvement ratio of ${value.ratio}%1.4f.\n")
  }

  def benchmark(algo: Algo, formulas: List[Formula]): Improvement = algo match {
    case Algo.OCBSL => benchmarkOcbsl(formulas)
    case Algo.OL => benchmarkOl(formulas)
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
}
