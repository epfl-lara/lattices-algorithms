import jdk.nashorn.internal.ir.Assignment

import scala.collection.mutable

object Datastructures {

  sealed abstract class Formula {
    val size: Int

    override def toString: String = Printer.pretty(this)
  }
  case class Variable(id: Int) extends Formula {
    val size = 1
  }
  case class Neg(child: Formula) extends Formula {
    val size: Int = child.size
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

  trait EquivalenceAndNormalForAlgorithm {
    def isSame(formula1: Formula, formula2: Formula): Boolean
    def reducedForm(formula: Formula): Formula
  }

  def memoize[I, O](f: I => O): I => O = new mutable.HashMap[I, O]() {
    override def apply(key: I): O = getOrElseUpdate(key, f(key))
  }

  def negationNormalForm(f: Formula, positive: Boolean = true): Formula = f match {
    case Variable(id) => if (positive) Variable(id) else Neg(Variable(id))
    case Neg(child) => negationNormalForm(child, !positive)
    case Or(children) => if positive then Or(children.map(c => negationNormalForm(c))) else And(children.map(c => negationNormalForm(c, false)))
    case And(children) => if positive then And(children.map(c => negationNormalForm(c))) else Or(children.map(c => negationNormalForm(c, false)))
    case Literal(b) => Literal(positive == b)
  }

}
