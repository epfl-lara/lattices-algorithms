import Datastructures.*
import LatticesAlgorithm.NNFFormula
import LatticesAlgorithm.{toFormula => toFormulaOl}
import OcbslAlgorithm.NoAndFormula
import OcbslAlgorithm.NormalFormula
import OcbslAlgorithm.{toFormula => toFormulaOcbsl}
object Printer {

  def pretty(f: Formula): String = f match
    case Variable(id) => s"x$id"
    case Neg(child) => s"!${pretty(child)}"
    case Or(children) => if children.size == 2 then s"(${pretty(children(0))} ∨ ${pretty(children(1))})" else s"or(${children.map(pretty).mkString(", ")})"
    case And(children) => if children.size == 2 then s"(${pretty(children(0))} ∧ ${pretty(children(1))})" else s"and(${children.map(pretty).mkString(", ")})"
    case Literal(b) => if b then "1" else "0"

  def pretty(f: NormalFormula): String = f match
    case OcbslAlgorithm.NVariable(id, code) => s"x$id"
    case OcbslAlgorithm.NNeg(child, code) => s"!${pretty(child)}"
    case OcbslAlgorithm.NOr(children, code) => if children.size == 2 then s"(${pretty(children(0))} ∨ ${pretty(children(1))})" else s"or(${children.map(pretty).mkString(", ")})"
    case OcbslAlgorithm.NLiteral(b) => if b then "1" else "0"

  def pretty(f: NNFFormula): String = f match
    case LatticesAlgorithm.NNFVariable(id, polarity) => if polarity then s"x$id" else s"!x$id"
    case LatticesAlgorithm.NNFOr(children) => if children.size == 2 then s"(${pretty(children(0))} ∨ ${pretty(children(1))})" else s"or(${children.map(pretty).mkString(", ")})"
    case LatticesAlgorithm.NNFAnd(children) => if children.size == 2 then s"(${pretty(children(0))} ∧ ${pretty(children(1))})" else s"and(${children.map(pretty).mkString(", ")})"
    case LatticesAlgorithm.NNFLiteral(b) => if b then "1" else "0"

  def pretty(f: NoAndFormula): String = f match
    case OcbslAlgorithm.NoAndVariable(id) => s"x$id"
    case OcbslAlgorithm.NoAndNeg(child) => s"!${pretty(child)}"
    case OcbslAlgorithm.NoAndOr(children) => if children.size == 2 then s"(${pretty(children(0))} ∨ ${pretty(children(1))})" else s"or(${children.map(pretty).mkString(", ")})"
    case OcbslAlgorithm.NoAndLiteral(b) => if b then "1" else "0"
}
