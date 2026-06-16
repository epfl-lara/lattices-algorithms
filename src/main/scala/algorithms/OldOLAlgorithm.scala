package ortholattices.algorithms

import Datastructures.*
import ortholattices.algorithms.OldOLAlgorithm.*
import com.zaxxer.sparsebits.SparseBitSet

import scala.collection.mutable

class OldOLAlgorithm extends EquivalenceAndNormalFormAlgorithm {
  import OLAlgorithm.*


  def latticesLEQ(formula1: NormalPFormula, formula2: NormalPFormula): Boolean =
    if formula1.uniqueKey == formula2.uniqueKey then true
    else formula1.oldLessThanCached(formula2) match
      case Some(value) => value
      case None =>
        val r = (formula1, formula2) match
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
        formula1.oldSetLessThanCache(formula2, r)
        r

  def simplify(children:List[NormalPFormula], polarity:Boolean): NormalPFormula = {
    val nonSimplified = NPAnd(children, polarity)
    var remaining : List[NormalPFormula] = Nil
    def treatChild(i:NormalPFormula): List[NormalPFormula] = {
      val r: List[NormalPFormula] = i match
        case NPAnd(ch, true) => ch
        case NPAnd(ch, false) =>
          if (polarity) {
            val trCh = ch map getInverse
            trCh.find(f =>{
              latticesLEQ(nonSimplified, f)
            }) match
              case Some(value) =>
                treatChild(value)
              case None => List(i)
          } else {
            val trCh = ch
            trCh.find(f =>{
              latticesLEQ(f, nonSimplified)
            }) match
              case Some(value) =>
                treatChild(getInverse(value))
              case None => List(i)
          }
        case _ => List(i)
      r
    }
    for (i <- children) do {
      val r = treatChild(i)
      remaining = r++remaining
    }
    var accepted: List[NormalPFormula] = Nil
    while remaining.nonEmpty do {
      val current = remaining.head
      remaining = remaining.tail
      if (!latticesLEQ(NPAnd(remaining++accepted, true), current)){
        accepted = current::accepted
      }
    }
    val r = if accepted.isEmpty then NPLiteral(polarity)
        else if accepted.size == 1 then if polarity then accepted.head else getInverse(accepted.head)
        else NPAnd(accepted, polarity)
    r
  }

  def checkForContradiction(f:NPAnd): Boolean = {
    f match
      case NPAnd(children, false) =>
        children.exists(cc => latticesLEQ(cc, f))
      case NPAnd(children, true) =>
        val shadowChildren = children map getInverse
        shadowChildren.exists(sc => latticesLEQ(f, sc))
  }

  def nPnormalForm(formula:PolarFormula):NormalPFormula = {
    formula.oldPolarNormalForm match
      case Some(value) =>
        value
      case None =>
        val r = formula match
          case PolarVariable(id, true) => NPVariable(id, true)
          case PolarVariable(id, false) =>
            getInverse(nPnormalForm(getInversePolar(formula)))
          case PolarAnd(children, polarity) =>
            val newChildren = children map nPnormalForm
            val simp = simplify(newChildren, polarity)
            simp match
              case disj: NPAnd if checkForContradiction(disj) => NPLiteral(!polarity)
              case _ => simp
          case PolarLiteral(b) => NPLiteral(b)
        formula.oldPolarNormalForm = Some(r)
        r
  }

  def checkEquivalence(formula1: PolarFormula, formula2: PolarFormula): Boolean =
    val a = nPnormalForm(formula1)
    val b = nPnormalForm(formula2)
    latticesLEQ(a, b) && latticesLEQ(b, a)
  override def isSame(formula1: Formula, formula2: Formula): Boolean = checkEquivalence(polarize(formula1), polarize(formula2))


  override def reducedForm(formula: Formula): Formula =
    val p = polarize(formula)
    val nf = nPnormalForm(p)
    val res = toFormula(nf)
    res
}

object OldOLAlgorithm extends EquivalenceAndNormalFormAlgorithm {
  import OLAlgorithm.*

  def nPnormalForm(p:PolarFormula): NormalPFormula = (new OldOLAlgorithm).nPnormalForm(p)
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
      case NPVariable(id, true) => Variable(id)
      case NPVariable(id, false) => Neg(toFormulaAIG(getInverse(f)))
      case NPAnd(children, polarity) =>
        if polarity then
          And(children.map(c => toFormulaAIG(c)))
        else
          Neg(And(children.map(c => toFormulaAIG(c))))
      case NPLiteral(b) => Literal(positive == b)
    f.formulaAIG = Some(r)
    r

  def toFormula(f:NormalPFormula): Formula = toFormulaNNF(f)

  //def checkEquivalence(formula1: PolarFormula, formula2: PolarFormula): Boolean = (new OldOLAlgorithm).checkEquivalence(formula1, formula2)

  override def isSame(formula1: Formula, formula2: Formula): Boolean = (new OldOLAlgorithm).isSame(formula1, formula2)

  override def reducedForm(formula: Formula): Formula = (new OldOLAlgorithm).reducedForm(formula)

  def polarize(f: Formula, polarity: Boolean = true): PolarFormula = {
    if polarity & f.oldPolarFormula.isDefined then return f.oldPolarFormula.get
    if !polarity & f.oldPolarFormula.isDefined then return getInversePolar(f.oldPolarFormula.get)
    val r = f match {
      case Variable(id) =>
        val nv = PolarVariable(id, polarity)
        nv
      case Neg(child) => polarize(child, !polarity)
      case Or(children) =>
        PolarAnd(children.map(polarize(_, false)), !polarity)
      case And(children) =>
        PolarAnd(children.map(polarize(_, true)), polarity)
      case Literal(b) => PolarLiteral(b == polarity)
    }
    if polarity then f.oldPolarFormula = Some(r)
    else f.oldPolarFormula = Some(getInversePolar(r))
    r
  }


    def getInversePolar(f: PolarFormula): PolarFormula = {
      f.oldPolarInverse match
        case Some(value) => value
        case None =>
          val second = f match
            case PolarVariable(id, polarity) => PolarVariable(id, !polarity)
            case PolarAnd(children, polarity) => PolarAnd(children, !polarity)
            case PolarLiteral(b) => PolarLiteral(!b)
          f.oldPolarInverse = Some(second)
          second.oldPolarInverse = Some(f)
          second
    }

  def getInverse(f: NormalPFormula): NormalPFormula = {
    f.inverse match
      case Some(value) => value
      case None =>
        val second = f match
          case NPVariable(id, polarity) => NPVariable(id, !polarity)
          case NPAnd(children, polarity) => NPAnd(children, !polarity)
          case NPLiteral(b) => NPLiteral(!b)
        f.inverse = Some(second)
        second.inverse = Some(f)
        second
  }

}