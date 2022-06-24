import Datastructures.*

import scala.util.Random

object FormulaGenerator {

  def randomFormula(size: Int, n: Int, seed: Option[Int] = None): Formula = {
    val variables = List.range(0, n).map(i => Variable(i))
    val rg: Random = seed match
      case Some(value) => new Random(value)
      case None => new Random()
    def single(size: Int): Formula = {
      if size <= 2 then {
        val v = variables(rg.nextInt(variables.size))
        if rg.nextBoolean() then v else Neg(v)
      } else
        val split = rg.nextInt(size - 1)
        val i1 = single(split+1)
        val i2 = single(size - split-2)
        val f = if rg.nextBoolean() then Or(List(i1, i2)) else And(List(i1, i2))
        negationNormalForm(f)
    }
    single(size)
  }

}
