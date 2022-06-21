import scala.collection.mutable

object Datastructures {

  sealed abstract class Formula {
    val size: Int
    var ocbslNormalForm: Option[NormalFormula] = None
    var olNormalForm: Option[NormalFormula] = None
  }
  case class Variable(id: String) extends Formula {
    val size = 1
  }
  case class Neg(child: Formula) extends Formula {
    val size: Int = 1 + child.size
  }
  case class Or(children: List[Formula]) extends Formula {
    val size: Int = (children map (_.size)).foldLeft(1) { case (a, b) => a + b }
  }
  case class And(children: List[Formula]) extends Formula {
    val size: Int = (children map (_.size)).foldLeft(1) { case (a, b) => a + b }
  }
  case class Literal(b: Boolean) extends Formula {
    val size = 1
  }


  sealed abstract class NormalFormula {
    val code:Int
  }
  case class NVariable(id: String, code:Int) extends NormalFormula
  case class NNeg(child: NormalFormula, code:Int) extends NormalFormula
  case class NOr(children: List[NormalFormula], code:Int) extends NormalFormula
  case class NLiteral(b: Boolean) extends NormalFormula{
    val code:Int = if (b) 1 else 0
  }



  trait EquivalenceAndNormalForAlgorithm {
    def isSame(formula1:Formula, formula2:Formula):Boolean
    def normalForm(formula:Formula):NormalFormula | Formula
  }

  def memoize[I, O](f: I => O): I => O = new mutable.HashMap[I, O]() {
    override def apply(key: I): O = getOrElseUpdate(key, f(key))
  }

}
