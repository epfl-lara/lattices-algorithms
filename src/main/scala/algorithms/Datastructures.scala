package ortholattices.algorithms

import ortholattices.algorithms.OLAlgorithm.PolarFormula
import ortholattices.algorithms.OcbslAlgorithm.NoOrFormula
import ortholattices.algorithms.Printer

import scala.collection.mutable

object Datastructures {

  var totalNumberFormula: Int = 0

  sealed abstract class Formula {
    val size: BigInt
    totalNumberFormula += 1
    val uniqueKey: Int = totalNumberFormula

    var noOrFormulaP: Option[NoOrFormula] = None
    var noOrFormulaN: Option[NoOrFormula] = None

    var polarFormula: Option[PolarFormula] = None

    lazy val circuitSize: BigInt = Datastructures.circuitSize(List(this))

    override def toString: String = Printer.pretty(this)

    val depth: Int = this match
      case Variable(id) => 1
      case Neg(child) => child.depth
      case Or(children) => 1 + children.map(_.depth).max
      case And(children) => 1 + children.map(_.depth).max
      case Literal(b) => 1
  }

  case class Variable(id: Int) extends Formula {
    val size = 1
  }

  case class Neg(child: Formula) extends Formula {
    val size: BigInt = child.size
  }

  case class Or(children: List[Formula]) extends Formula {
    val size: BigInt = {
      val fold = (children map (_.size)).sum + 1
      fold
    }
  }

  case class And(children: List[Formula]) extends Formula {
    val size: BigInt = {
      val fold = (children map (_.size)).sum + 1
      fold
    }
  }

  case class Literal(b: Boolean) extends Formula {
    val size = 1
  }

  def redo(f: Formula): Formula = f match
    case Variable(id) => Variable(id)
    case Neg(child) => Neg(redo(child))
    case Or(children) => Or(children map redo)
    case And(children) => And(children map redo)
    case Literal(b) => Literal(b)


  trait EquivalenceAndNormalFormAlgorithm {
    def isSame(formula1: Formula, formula2: Formula): Boolean
    def reducedForm(formula: Formula): Formula
  }
  
  def negationNormalForm(f: Formula, positive: Boolean = true): Formula = f match {
    case Variable(id) => if (positive) Variable(id) else Neg(Variable(id))
    case Neg(child) => negationNormalForm(child, !positive)
    case Or(children) => if positive then Or(children.map(c => negationNormalForm(c))) else And(children.map(c => negationNormalForm(c, false)))
    case And(children) => if positive then And(children.map(c => negationNormalForm(c))) else Or(children.map(c => negationNormalForm(c, false)))
    case Literal(b) => Literal(positive == b)
  }

  def flatten(f: Formula): Formula = f match
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


  def circuitSize(fs: List[Formula]): BigInt = {
    val s: mutable.HashMap[Int, Int] = mutable.HashMap.empty

    def foldedSize(f1: Formula): BigInt = {
      if s.contains(f1.uniqueKey) then
        s.update(f1.uniqueKey, s(f1.uniqueKey) + 1)
        0: BigInt
      else
        s.update(f1.uniqueKey, 1)
        f1 match
          case Variable(id) => 1
          case Neg(child) => foldedSize(child)
          case Or(children) =>
            //val fold = (children map (foldedSize)).foldLeft(-1:BigInt) { case (a, b) => (a + b+1) }
            val fold = (children map (foldedSize)).sum + 1
            fold
          case And(children) =>
            //val fold = (children map (foldedSize)).foldLeft(-1:BigInt) { case (a, b) => (a + b+1) }
            val fold = (children map (foldedSize)).sum + 1
            fold
          case Literal(b) => 1
    }

    val r = (fs map foldedSize).sum
    //val m: List[(Int, Int)] = s.values.groupBy(c => c).map(l => (l.head, l.size)).toList
    //m.sortBy(_._1).foreach{c =>println(s"${c._2} nodes where linked ${c._1} times")}
    r
  }
}
