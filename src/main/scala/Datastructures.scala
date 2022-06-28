import OLAlgorithm.PolarFormula
import OcbslAlgorithm.NoAndFormula
import jdk.nashorn.internal.ir.Assignment

import scala.collection.mutable

object Datastructures {

  var totalNumberFormula : Int = 0
  sealed abstract class Formula {
    val size: BigInt
    totalNumberFormula+=1

    var noAndFormulaP:Option[NoAndFormula] = None
    var noAndFormulaN:Option[NoAndFormula] = None

    var polarFormulaP:Option[PolarFormula] = None
    var polarFormulaN:Option[PolarFormula] = None

    override def toString: String = Printer.pretty(this)
  }
  case class Variable(id: Int) extends Formula {
    val size = 1
  }
  case class Neg(child: Formula) extends Formula {
    val size: BigInt = child.size
  }
  case class Or(children: List[Formula]) extends Formula {
    val size: BigInt = {
      val fold = (children map (_.size)).foldLeft(1:BigInt) { case (a, b) => (a + b) }
      if children.exists(_.isInstanceOf[Or]) then fold else fold+1
    }
  }
  case class And(children: List[Formula]) extends Formula {
    val size: BigInt = {
      val fold = (children map (_.size)).foldLeft(1:BigInt) { case (a, b) => (a + b) }
      if children.exists(_.isInstanceOf[And]) then fold else fold+1
    }
  }


  case class Literal(b: Boolean) extends Formula {
    val size = 1
  }

  trait EquivalenceAndNormalFormAlgorithm {
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

  def flatten(f:Formula): Formula = f match
    case Or(children) =>
      val nc: List[Formula] = children map flatten
      Or(nc.flatMap {
        case Or(children) => children
        case c => List(c)
      })
    case And(children) =>
      val nc: List[Formula] = children map flatten
      And(nc.flatMap {
        case And(children) => children
        case c => List(c)
      })
    case _ => f

}
