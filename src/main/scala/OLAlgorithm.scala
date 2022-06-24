import Datastructures.*

import scala.collection.mutable

class OLAlgorithm extends EquivalenceAndNormalFormAlgorithm {
  import OLAlgorithm.*

  var invTot = 0
  def getInverse(f: NormalPFormula): NormalPFormula = {
    invTot+=1
    f.inverse match
      case Some(value) => value
      case None =>
        val second = f match
          case NPVariable(id, polarity) => NPVariable(id, !polarity)
          case NPOr(children, polarity) => NPOr(children, !polarity)
          case NPLiteral(b) => NPLiteral(!b)
        f.inverse = Some(second)
        second.inverse = Some(f)
        second
  }

  var leqTot = 0
  def latticesLEQ(formula1: NormalPFormula, formula2: NormalPFormula): Boolean =
    leqTot+=1
    formula1.lessThan.get(formula2) match
      case Some(value) => value
      case None => val r = (formula1, formula2) match
        case (NPLiteral(b1), NPLiteral(b2)) => !b1 || b2
        case (NPLiteral(b), _) => !b
        case (_, NPLiteral(b)) => b
        case (NPVariable(id1, polarity1), NPVariable(id2, polarity2)) =>
          id1 == id2 && polarity1 == polarity2
        case (_, NPOr(children, false)) =>
          children.forall(c => latticesLEQ(formula1, getInverse(c)))
        case (NPOr(children, true), _) =>
          children.forall(c => latticesLEQ(c, formula2))
        case (v: NPVariable, NPOr(children, true)) =>
          children.exists(c => latticesLEQ(v, c))
        case (NPOr(children, false), v: NPVariable) =>
          children.exists(c => latticesLEQ(getInverse(c), v))
        case (NPOr(children1, false), NPOr(children2, true)) =>
          children1.exists(c => latticesLEQ(getInverse(c), formula2)) || children2.exists(c => latticesLEQ(formula1, c))
        r

  def simplify(children:List[NormalPFormula], polarity:Boolean): NormalPFormula = {
    var remaining : List[NormalPFormula] = Nil
    for (i <- children) do {
      i match
        case NPOr(ch, true) => remaining = ch++remaining
        case _ => remaining = i::remaining
    }
    var accepted: List[NormalPFormula] = Nil
    while remaining.nonEmpty do {
      val current = remaining.head
      remaining = remaining.tail
      if (!remaining.exists(e => latticesLEQ(current, e)) &&
          !accepted.exists(e => latticesLEQ(current, e))) {
        accepted = current :: accepted
      }
    }
    if accepted.isEmpty then NPLiteral(!polarity)
    else if accepted.size == 1 then if polarity then accepted.head else getInverse(accepted.head)
    else NPOr(accepted, polarity)
  }

  def checkForContradiction(f:NPOr): Boolean = f match
    case NPOr(children, true) =>
      val shadowChildren = children map getInverse
      shadowChildren.exists(sc => latticesLEQ(sc, f))
    case NPOr(children, false) =>
      children.exists(c => latticesLEQ(f, c))


  var norTot = 0
  def nPnormalForm(formula:PolarFormula):NormalPFormula = {
    norTot+=1
    formula.polarNormalForm match
      case Some(value) => value
      case None =>
        val r = formula match
          case PolarVariable(id, polarity) => NPVariable(id, polarity)
          case PolarOr(children, polarity) =>
            val newChildren = children map nPnormalForm
            //println(s"before simp: $newChildren, polarity: $polarity")
            val simp = simplify(newChildren, polarity)
            //println(s"after simp: $simp")
            simp match
              case disj: NPOr if checkForContradiction(disj) => NPLiteral(polarity)
              case _ => simp
          case PolarLiteral(b) => NPLiteral(b)
        formula.polarNormalForm = Some(r)
        r
  }


  def checkEquivalence(formula1: PolarFormula, formula2: PolarFormula): Boolean =
    val a = nPnormalForm(formula1)
    val b = nPnormalForm(formula2)
    latticesLEQ(a, b) & latticesLEQ(b, a)

  override def isSame(formula1: Formula, formula2: Formula): Boolean = checkEquivalence(polarize(formula1), polarize(formula2))


  override def reducedForm(formula: Formula): Formula =
    val p = polarize(formula)
    val res = toFormula(nPnormalForm(p))
    val n = p.size
    val squared = n*n
    println(s"     Stats: Polarized formula of size $n (squared: $squared)")
    println(s"     norTot: $norTot (${norTot.toDouble/squared}), leqTot: $leqTot (${leqTot.toDouble/squared}), invTot: $invTot (${invTot.toDouble/squared})")
    res
}

object OLAlgorithm extends EquivalenceAndNormalFormAlgorithm {

  sealed abstract class PolarFormula {
    val size: Int
    var polarNormalForm: Option[NormalPFormula] = None
    override def toString: String = Printer.pretty(this)
  }
  case class PolarVariable(id: Int, polarity:Boolean = true) extends PolarFormula {
    val size = 1
  }
  case class PolarOr(children: List[PolarFormula], polarity:Boolean = true) extends PolarFormula {
    val size: Int = (children map (_.size)).foldLeft(1) { case (a, b) => a + b }
  }
  case class PolarLiteral(b: Boolean) extends PolarFormula {
    val size = 1
  }

  sealed abstract class NormalPFormula {
    //val code: Int
    override def toString: String = Printer.pretty(this)
    var inverse: Option[NormalPFormula] = None
    val lessThan: mutable.HashMap[NormalPFormula, Boolean] = mutable.HashMap(this -> true)
  }
  case class NPVariable(id: Int, polarity:Boolean) extends NormalPFormula
  case class NPOr(children: List[NormalPFormula], polarity:Boolean) extends NormalPFormula
  case class NPLiteral(b: Boolean) extends NormalPFormula

  /**
   * Puts back in regular formula syntax, and performs negation normal form to produce shorter version.
   */
  def toFormula(f: NormalPFormula, positive: Boolean = true): Formula = f match
    case NPVariable(id, polarity) => if positive==polarity then Variable(id) else Neg(Variable(id))
    case NPOr(children, polarity) => if positive==polarity then Or(children.map(c => toFormula(c, true))) else And(children.map(c => toFormula(c, false)))
    case NPLiteral(b) => Literal(positive == b)



  def checkEquivalence(formula1: PolarFormula, formula2: PolarFormula): Boolean = (new OLAlgorithm).checkEquivalence(formula1, formula2)

  override def isSame(formula1: Formula, formula2: Formula): Boolean = (new OLAlgorithm).isSame(formula1, formula2)

  override def reducedForm(formula: Formula): Formula = (new OLAlgorithm).reducedForm(formula)

  def polarize(f: Formula, polarity: Boolean = true): PolarFormula = f match {
    case Variable(id) => PolarVariable(id, polarity)
    case Neg(child) => polarize(child, !polarity)
    case Or(children) =>
      PolarOr(children.map(polarize(_, true)), polarity)
    case And(children) =>
      PolarOr(children.map(polarize(_, false)), !polarity)
    case Literal(b) => PolarLiteral(b == polarity)
  }


}