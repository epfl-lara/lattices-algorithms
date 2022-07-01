import Datastructures.*
object Printer {

  def prettyFull(f: Formula): String = f match
    case Variable(id) => s"x$id"
    case Neg(child) => s"neg(${prettyFull(child)})"
    case Or(children) => s"or(${children.map(prettyFull).mkString(", ")})"
    case And(children) => s"and(${children.map(prettyFull).mkString(", ")})"
    case Literal(b) => if b then "1" else "0"

  def pretty(f: Formula): String = f match
    case Variable(id) => s"x$id"
    case Neg(child) => s"!${pretty(child)}"
    case Or(children) => s"or(${children.map(pretty).mkString(", ")})"
    case And(children) => s"and(${children.map(pretty).mkString(", ")})"
    case Literal(b) => if b then "1" else "0"

  def pretty(f: OcbslAlgorithm.NormalFormula): String = f match
    case OcbslAlgorithm.NVariable(id, code) => s"x$id"
    case OcbslAlgorithm.NNeg(child, code) => s"!${pretty(child)}"
    case OcbslAlgorithm.NAnd(children, code) => if children.size == 2 then s"(${pretty(children(0))} ∨ ${pretty(children(1))})" else s"or(${children.map(pretty).mkString(", ")})"
    case OcbslAlgorithm.NLiteral(b) => if b then "1" else "0"

  def pretty(f: OcbslAlgorithm.NoOrFormula): String = f match
    case OcbslAlgorithm.NoOrVariable(id) => s"x$id"
    case OcbslAlgorithm.NoOrNeg(child) => s"!${pretty(child)}"
    case OcbslAlgorithm.NoOrAnd(children) => if children.size == 2 then s"(${pretty(children(0))} ∨ ${pretty(children(1))})" else s"or(${children.map(pretty).mkString(", ")})"
    case OcbslAlgorithm.NoOrLiteral(b) => if b then "1" else "0"

  def pretty(f: OLAlgorithm.NormalPFormula): String = f match
    case OLAlgorithm.NPVariable(id, polarity) => if polarity then s"x$id" else s"!x$id"
    case OLAlgorithm.NPOr(children, polarity) =>
      val inner = if children.size == 2 then s"(${pretty(children(0))} ∨ ${pretty(children(1))})" else s"or(${children.map(pretty).mkString(", ")})"
      if polarity then inner else s"!$inner"
    case OLAlgorithm.NPLiteral(b) => if b then "1" else "0"

  def pretty(f: OLAlgorithm.PolarFormula): String = f match
    case OLAlgorithm.PolarVariable(id, polarity) => if polarity then s"x$id" else s"!x$id"
    case OLAlgorithm.PolarOr(children, polarity) =>
      val inner = if children.size == 2 then s"(${pretty(children(0))} ∨ ${pretty(children(1))})" else s"or(${children.map(pretty).mkString(", ")})"
      if polarity then inner else s"!$inner"
    case OLAlgorithm.PolarLiteral(b) => if b then "1" else "0"
}
