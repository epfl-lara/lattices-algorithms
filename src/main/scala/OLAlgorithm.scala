import Datastructures.*

import scala.collection.mutable

class OLAlgorithm extends EquivalenceAndNormalFormAlgorithm {
  import OLAlgorithm.*

  var invTot = 0
  var invTotTrue = 0
  val v = NPVariable(-5, true)
  def getInverse(f: NormalPFormula): NormalPFormula = {
    invTot+=1
    f.inverse match
      case Some(value) => value
      case None =>
        invTotTrue+=1
        val second = f match
          case NPVariable(id, polarity) => NPVariable(id, !polarity)
          case NPAnd(children, polarity) => NPAnd(children, !polarity)
          case NPLiteral(b) => NPLiteral(!b)
        f.inverse = Some(second)
        second.inverse = Some(f)
        second
  }

  var leqTot = 0
  var leqTotTrue = 0
  def latticesLEQ(formula1: NormalPFormula, formula2: NormalPFormula): Boolean =
    leqTot+=1

    formula1.lessThan.get(formula2.uniqueKey) match
      case Some(value) => value
      case None =>

        leqTotTrue+=1
        val r = (formula1, formula2) match
          case (_, NPVariable(-5, true)) => false
          case (NPVariable(-5, true), _) => false
          case (NPLiteral(b1), NPLiteral(b2)) => !b1 || b2
          case (NPLiteral(b), _) => !b
          case (_, NPLiteral(b)) => b
          case (NPVariable(id1, polarity1), NPVariable(id2, polarity2)) =>
            id1 == id2 && polarity1 == polarity2
          case (_, NPAnd(children, true)) =>
            children.forall(c => latticesLEQ(formula1, c))
          case (NPAnd(children, false), _) =>
            children.forall(c => latticesLEQ(getInverse(c), formula2))
          case (v: NPVariable, NPAnd(children, false)) =>
            children.exists(c => latticesLEQ(v, getInverse(c)))
          case (NPAnd(children, true), v: NPVariable) =>
            children.exists(c => latticesLEQ(c, v))
          case (NPAnd(children1, true), NPAnd(children2, false)) =>
            children1.exists(c => latticesLEQ(c, formula2)) || children2.exists(c => latticesLEQ(formula1, getInverse(c)))
        formula1.lessThan.update(formula2.uniqueKey, r)
        r

  def simplify(children:List[NormalPFormula], polarity:Boolean): NormalPFormula = {
    var remaining : List[NormalPFormula] = Nil
    for (i <- children) do {
      i match
        case NPAnd(ch, true) => remaining = ch++remaining
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
    if accepted.isEmpty then NPLiteral(polarity)
    else if accepted.size == 1 then if polarity then accepted.head else getInverse(accepted.head)
    else NPAnd(accepted, polarity)
  }

  def checkForContradiction(f:NPAnd): Boolean = {
    f match
      case NPAnd(children, false) =>
        children.exists(cc => latticesLEQ(cc, f))
      case NPAnd(children, true) =>
        val shadowChildren = children map getInverse
        shadowChildren.exists(sc => latticesLEQ(f, sc))
  }

  var norTot = 0
  def nPnormalForm(formula:PolarFormula):NormalPFormula = {
    norTot+=1
    //if norTot%100 == 0 then println(s"norTot: $norTot")
    //else if norTot> 4800 && norTot%10 == 0 then println(s"norTot: $norTot")
    formula.polarNormalForm match
      case Some(value) => value
      case None =>
        val r = formula match
          case PolarVariable(id, polarity) => NPVariable(id, polarity)
          case PolarAnd(children, polarity) =>
            val newChildren = children map nPnormalForm
            val simp = simplify(newChildren, polarity)
            simp match
              case disj: NPAnd if checkForContradiction(disj) => NPLiteral(!polarity)
              case _ => simp
          case PolarLiteral(b) => NPLiteral(b)
        formula.polarNormalForm = Some(r)
        r
  }

  def checkEquivalence(formula1: PolarFormula, formula2: PolarFormula): Boolean =
    val a = nPnormalForm(formula1)
    val b = nPnormalForm(formula2)
    latticesLEQ(a, b) & latticesLEQ(b, a)
  override def isSame(formula1: Formula, formula2: Formula): Boolean = false //checkEquivalence(polarize(formula1), polarize(formula2))


  override def reducedForm(formula: Formula): Formula =
    val p = polarize(formula)
    val nf = nPnormalForm(p)
    val res = toFormula(nf)
    val n = p.size
    val squared = n*n
    //println(s"     Stats: Polarized formula of size $n (squared: $squared)")
    //println(s"     norTot: $norTot (${norTot.toDouble/squared}), leqTot: $leqTot (${leqTot.toDouble/squared}), invTot: $invTot (${invTot.toDouble/squared})")
    res
}

object OLAlgorithm extends EquivalenceAndNormalFormAlgorithm {

  var totPolarFormula = 0
  sealed abstract class PolarFormula {
    val uniqueKey:Int = totPolarFormula
    val size: Int
    var polarNormalForm: Option[NormalPFormula] = None
    override def toString: String = Printer.pretty(this)
    totPolarFormula += 1
  }
  case class PolarVariable(id: Int, polarity:Boolean = true) extends PolarFormula {
    val size = 1
  }
  case class PolarAnd(children: List[PolarFormula], polarity:Boolean = true) extends PolarFormula {
    val size: Int = (children map (_.size)).foldLeft(1) { case (a, b) => a + b }
  }
  case class PolarLiteral(b: Boolean) extends PolarFormula {
    val size = 1
  }

  var totNormalPFormula = 0
  sealed abstract class NormalPFormula {
    totNormalPFormula+=1
    val uniqueKey:Int = totNormalPFormula
    //val code: Int
    var formulaP: Option[Formula] = None
    var formulaN: Option[Formula] = None
    var formulaAIG: Option[Formula] = None
    override def toString: String = Printer.pretty(this)
    var inverse: Option[NormalPFormula] = None
    val lessThan: mutable.HashMap[Int, Boolean] = mutable.HashMap(uniqueKey -> true)


    override def equals(obj: Any): Boolean = obj match
      case f: NormalPFormula => eq(f)
      case _ => super.equals(obj)
  }
  case class NPVariable(id: Int, polarity:Boolean) extends NormalPFormula
  case class NPAnd(children: List[NormalPFormula], polarity:Boolean) extends NormalPFormula
  case class NPLiteral(b: Boolean) extends NormalPFormula

  def nPnormalForm(p:PolarFormula): NormalPFormula = (new OLAlgorithm).nPnormalForm(p)
  /**
   * Puts back in regular formula syntax, and performs negation normal form to produce shorter version.
   */
  def toFormulaNNF(f: NormalPFormula, positive: Boolean = true): Formula =
    if positive then
      if f.formulaP.isDefined then return f.formulaP.get
      else if f.inverse.isDefined && f.inverse.get.formulaN.isDefined then return f.inverse.get.formulaN.get
    if !positive then
      if f.formulaN.isDefined then return f.formulaN.get
      else if f.inverse.isDefined && f.inverse.get.formulaP.isDefined then return f.inverse.get.formulaP.get
    val r = f match
      case NPVariable(id, polarity) => if positive==polarity then Variable(id) else Neg(toFormulaNNF(f, !positive))
      case NPAnd(children, polarity) =>
        if positive==polarity then
          And(children.map(c => toFormulaNNF(c, true)))
        else
          Or(children.map(c => toFormulaNNF(c, false)))
      case NPLiteral(b) => Literal(positive == b)
    if positive then f.formulaP = Some(r)
    else  f.formulaN = Some(r)
    r

  /**
   * Puts back in regular formula syntax, in an AIG (without disjunctions) format
   */
  def toFormulaAIG(f: NormalPFormula, positive: Boolean = true): Formula =
    if f.formulaAIG.isDefined then return f.formulaAIG.get
    val r = f match
      case NPVariable(id, polarity) => if polarity then Variable(id) else Neg(Variable(id))
      case NPAnd(children, polarity) =>
        if polarity then
          And(children.map(c => toFormulaAIG(c)))
        else
          Neg(And(children.map(c => toFormulaAIG(c))))
      case NPLiteral(b) => Literal(positive == b)
    f.formulaAIG = Some(r)
    r

  def toFormula(f:NormalPFormula): Formula = toFormulaAIG(f)

  //def checkEquivalence(formula1: PolarFormula, formula2: PolarFormula): Boolean = (new OLAlgorithm).checkEquivalence(formula1, formula2)

  override def isSame(formula1: Formula, formula2: Formula): Boolean = (new OLAlgorithm).isSame(formula1, formula2)

  override def reducedForm(formula: Formula): Formula = (new OLAlgorithm).reducedForm(formula)

  def polarize(f: Formula, polarity: Boolean = true): PolarFormula =
    if polarity & f.polarFormulaP.isDefined then return f.polarFormulaP.get
    if !polarity & f.polarFormulaN.isDefined then return f.polarFormulaN.get
    val r = f match {
      case Variable(id) => PolarVariable(id, polarity)
      case Neg(child) => polarize(child, !polarity)
      case Or(children) =>
        PolarAnd(children.map(polarize(_, false)), !polarity)
      case And(children) =>
        PolarAnd(children.map(polarize(_, true)), polarity)
      case Literal(b) => PolarLiteral(b == polarity)
    }
    if polarity then f.polarFormulaP = Some(r)
    else f.polarFormulaN = Some(r)
    r


}