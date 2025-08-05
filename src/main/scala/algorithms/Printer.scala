package ortholattices.algorithms
import ortholattices.algorithms.Datastructures.*

object Printer {

  /**
   * Allows to write or not the key before each node of the formula.
   * Usefull to debug structure sharing optimizations
   */
  def o(s: String): String = if writeKeys then s else ""

  var writeKeys: Boolean = false

  def prettyFull(f: Formula): String = f match
    case Variable(id) => s"x$id"
    case Neg(child) => s"neg(${prettyFull(child)})"
    case Or(children) => s"or(${children.map(prettyFull).mkString(", ")})"
    case And(children) => s"and(${children.map(prettyFull).mkString(", ")})"
    case Literal(b) => if b then "⊤" else "⊥"

  def pretty(f: Formula): String = o(s"${f.uniqueKey}") + (f match
    case Variable(id) => s"x$id"
    case Neg(child) => s"!${pretty(child)}"
    case Or(children) => s"or(${children.map(pretty).mkString(", ")})"
    case And(children) => s"and(${children.map(pretty).mkString(", ")})"
    case Literal(b) => if b then "⊤" else "⊥"
    )

  def pretty(f: OcbslAlgorithm.NormalFormula): String = o(s"${f.uniqueKey}") + (f match
    case OcbslAlgorithm.NVariable(id, code) => s"x$id"
    case OcbslAlgorithm.NNeg(child, code) => s"!${pretty(child)}"
    case OcbslAlgorithm.NAnd(children, code) => if children.size == 2 then s"(${pretty(children(0))} ∨ ${pretty(children(1))})" else s"or(${children.map(pretty).mkString(", ")})"
    case OcbslAlgorithm.NLiteral(b) => if b then "T" else "F"
    )

  def pretty(f: OcbslAlgorithm.NoOrFormula): String = o(s"${f.uniqueKey}") + (f match
    case OcbslAlgorithm.NoOrVariable(id) => s"x$id"
    case OcbslAlgorithm.NoOrNeg(child) => s"!${pretty(child)}"
    case OcbslAlgorithm.NoOrAnd(children) => s"and(${children.map(pretty).mkString(", ")})"
    case OcbslAlgorithm.NoOrLiteral(b) => if b then "T" else "F"
    )

  def pretty(f: OLAlgorithm.NormalPFormula): String = o(s"${f.uniqueKey}") + (f match
    case OLAlgorithm.NPVariable(id, polarity) => if polarity then s"x$id" else s"!x$id"
    case OLAlgorithm.NPAnd(children, polarity) =>
      val inner = s"and(${children.map(pretty).mkString(", ")})"
      if polarity then inner else s"!$inner"
    case OLAlgorithm.NPLiteral(b) => if b then "T" else "F"
    )

  def pretty(f: OLAlgorithm.PolarFormula): String = o(s"${f.uniqueKey}") + (f match
    case OLAlgorithm.PolarVariable(id, polarity) => if polarity then s"x$id" else s"!x$id"
    case OLAlgorithm.PolarAnd(children, polarity) =>
      val inner = s"and(${children.map(pretty).mkString(", ")})"
      if polarity then inner else s"!$inner"
    case OLAlgorithm.PolarLiteral(b) => if b then "T" else "F"
    )
}
