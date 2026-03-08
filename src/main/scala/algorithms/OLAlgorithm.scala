package ortholattices.algorithms

import Datastructures.*
import com.zaxxer.sparsebits.SparseBitSet

object OLAlgorithm extends EquivalenceAndNormalFormAlgorithm {

  var totPolarFormula = 0
  sealed abstract class PolarFormula {
    val uniqueKey:Int = totPolarFormula
    val size: Int
    var algo2PolarInverse: Option[PolarFormula] = None
    var oldPolarInverse: Option[PolarFormula] = None
    var algo2PolarNormalForm: Option[NormalPFormula] = None
    var oldPolarNormalForm: Option[NormalPFormula] = None
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
  case class PolarFunApplication(symbol: FunSymbol, args: List[PolarFormula], polarity: Boolean = true) extends PolarFormula {
    val size: Int = args.map(_.size).sum + 1
  }

  var totNormalPFormula = 0
  sealed abstract class NormalPFormula {
    totNormalPFormula+=1
    val uniqueKey:Int = totNormalPFormula
    var formulaP: Option[Formula] = None
    var formulaN: Option[Formula] = None
    var formulaAIG: Option[Formula] = None
    override def toString: String = Printer.pretty(this)
    var inverse: Option[NormalPFormula] = None

    private val lessThanBitSetA2:  SparseBitSet = new SparseBitSet
    private val lessThanBitSetOld: SparseBitSet = new SparseBitSet

    def a2LessThanCached(other: NormalPFormula): Option[Boolean] =
      val otherIx = 2 * other.uniqueKey
      if lessThanBitSetA2.get(otherIx) then Some(lessThanBitSetA2.get(otherIx + 1))
      else None

    def a2SetLessThanCache(other: NormalPFormula, value: Boolean): Unit =
      val otherIx = 2 * other.uniqueKey
      lessThanBitSetA2.set(otherIx)
      if value then lessThanBitSetA2.set(otherIx + 1)

    def oldLessThanCached(other: NormalPFormula): Option[Boolean] =
      val otherIx = 2 * other.uniqueKey
      if lessThanBitSetOld.get(otherIx) then Some(lessThanBitSetOld.get(otherIx + 1))
      else None

    def oldSetLessThanCache(other: NormalPFormula, value: Boolean): Unit =
      val otherIx = 2 * other.uniqueKey
      lessThanBitSetOld.set(otherIx)
      if value then lessThanBitSetOld.set(otherIx + 1)

    override def equals(obj: Any): Boolean = obj match
      case f: NormalPFormula => eq(f)
      case _ => super.equals(obj)
  }
  case class NPVariable(id: Int, polarity:Boolean) extends NormalPFormula
  case class NPAnd(children: List[NormalPFormula], polarity:Boolean) extends NormalPFormula
  case class NPLiteral(b: Boolean) extends NormalPFormula
  case class NPFunApplication(symbol: FunSymbol, args: List[NormalPFormula], polarity: Boolean) extends NormalPFormula

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
      case NPFunApplication(sym, args, polarity) =>
        val base = FunApplication(sym, args.map(a => toFormulaNNF(a, true)))
        if positive == polarity then base else Neg(base)
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
      case NPFunApplication(sym, args, polarity) =>
        val base = FunApplication(sym, args.map(toFormulaAIG(_)))
        if polarity then base else Neg(base)
      case NPLiteral(b) => Literal(positive == b)
    f.formulaAIG = Some(r)
    r

  def toFormula(f:NormalPFormula): Formula = toFormulaNNF(f)

  override def isSame(formula1: Formula, formula2: Formula): Boolean = (new OLAlgorithmStructural).isSame(formula1, formula2)

  override def reducedForm(formula: Formula): Formula = (new OLAlgorithmStructural).reducedForm(formula)

  def getInverse(f: NormalPFormula): NormalPFormula = {
    f.inverse match
      case Some(value) => value
      case None =>
        val second = f match
          case NPVariable(id, polarity) => NPVariable(id, !polarity)
          case NPAnd(children, polarity) => NPAnd(children, !polarity)
          case NPFunApplication(sym, args, polarity) => NPFunApplication(sym, args, !polarity)
          case NPLiteral(b) => NPLiteral(!b)
        f.inverse = Some(second)
        second.inverse = Some(f)
        second
  }

}

/**
 * Old version of the OL normalization algorithm that flattens same-polarity
 * NPAnd children into flat lists. Kept for efficiency comparison with the
 * new tree-preserving OLAlgorithm.
 */
class OLAlgorithm extends EquivalenceAndNormalFormAlgorithm {
  import OLAlgorithm.*

  // Own inverse helper — shadows the shared OLAlgorithm.getInversePolar so
  // OLAlgorithm's PolarFormula objects never share inverse state with other
  // algorithm variants.
  private def getInversePolar(f: PolarFormula): PolarFormula =
    f.oldPolarInverse match
      case Some(value) => value
      case None =>
        val second = f match
          case PolarVariable(id, polarity)              => PolarVariable(id, !polarity)
          case PolarAnd(children, polarity)             => PolarAnd(children, !polarity)
          case PolarFunApplication(sym, args, polarity) => PolarFunApplication(sym, args, !polarity)
          case PolarLiteral(b)                          => PolarLiteral(!b)
        f.oldPolarInverse = Some(second)
        second.oldPolarInverse = Some(f)
        second

  var leqCount: Long = 0

  def latticesLEQ(formula1: NormalPFormula, formula2: NormalPFormula): Boolean =
    leqCount += 1
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
          case (NPFunApplication(sym1, a1, p1), NPFunApplication(sym2, a2, p2)) =>
            sym1 == sym2 && p1 == p2 && a1.zip(a2).zip(sym1.variances).forall { case ((s, t), v) =>
              val effectiveV = if p1 then v else v.flip
              effectiveV match
                case Variance.Covariant => latticesLEQ(s, t)
                case Variance.Contravariant => latticesLEQ(t, s)
                case Variance.Invariant => latticesLEQ(s, t) && latticesLEQ(t, s)
            }
          case (_, NPAnd(children, true)) =>
            children.forall(c => latticesLEQ(formula1, c))
          case (NPAnd(children, false), _) =>
            children.forall(c => latticesLEQ(getInverse(c), formula2))
          case (_: NPVariable | _: NPFunApplication, NPAnd(children, false)) =>
            children.exists(c => latticesLEQ(formula1, getInverse(c)))
          case (NPAnd(children, true), _: NPVariable | _: NPFunApplication) =>
            children.exists(c => latticesLEQ(c, formula2))
          case (NPAnd(children1, true), NPAnd(children2, false)) =>
            children1.exists(c => latticesLEQ(c, formula2)) || children2.exists(c => latticesLEQ(formula1, getInverse(c)))
          case _ => false
        formula1.oldSetLessThanCache(formula2, r)
        r

  def simplify(children: List[NormalPFormula], polarity: Boolean): NormalPFormula = {
    val nonSimplified = NPAnd(children, polarity)
    var remaining: List[NormalPFormula] = Nil
    def treatChild(i: NormalPFormula): List[NormalPFormula] = {
      val r: List[NormalPFormula] = i match
        case NPAnd(ch, true) => ch.flatMap(treatChild)
        case NPAnd(ch, false) =>
          if (polarity) {
            val trCh = ch map getInverse
            trCh.find(f => {
              latticesLEQ(nonSimplified, f)
            }) match
              case Some(value) =>
                treatChild(value)
              case None => List(i)
          } else {
            val trCh = ch
            trCh.find(f => {
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
      remaining = r ++ remaining
    }
    var accepted: List[NormalPFormula] = Nil
    while remaining.nonEmpty do {
      val current = remaining.head
      remaining = remaining.tail
      if (!latticesLEQ(NPAnd(remaining ++ accepted, true), current)) {
        accepted = current :: accepted
      }
    }
    val r = if accepted.isEmpty then NPLiteral(polarity)
    else if accepted.size == 1 then if polarity then accepted.head else getInverse(accepted.head)
    else NPAnd(accepted, polarity)
    r
  }

  def checkForContradiction(f: NPAnd): Boolean = {
    f match
      case NPAnd(children, false) =>
        children.exists(cc => latticesLEQ(cc, f))
      case NPAnd(children, true) =>
        val shadowChildren = children map getInverse
        shadowChildren.exists(sc => latticesLEQ(f, sc))
  }

  def nPnormalForm(formula: PolarFormula): NormalPFormula = {
    formula.oldPolarNormalForm match
      case Some(value) =>
        value
      case None =>
        val r = formula match
          case PolarVariable(id, true) => NPVariable(id, true)
          case PolarVariable(id, false) =>
            getInverse(nPnormalForm(getInversePolar(formula)))
          case PolarFunApplication(sym, args, true) =>
            val newArgs = args.map(nPnormalForm)
            NPFunApplication(sym, newArgs, true)
          case PolarFunApplication(sym, args, false) =>
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

  /** Polarize using the old algorithm's own cache field on Formula.
   *  Flattens same-polarity PolarAnd children so that simplify always
   *  sees flat conjunction lists regardless of input nesting. */
  def oldPolarize(f: Formula, polarity: Boolean = true): PolarFormula = {
    if polarity & f.oldPolarFormula.isDefined then return f.oldPolarFormula.get
    if !polarity & f.oldPolarFormula.isDefined then return getInversePolar(f.oldPolarFormula.get)
    val r = f match {
      case Variable(id) => PolarVariable(id, polarity)
      case Neg(child) => oldPolarize(child, !polarity)
      case Or(children) =>
        val polarized = children.map(oldPolarize(_, false))
        val flat = polarized.flatMap {
          case PolarAnd(ch, true) => ch
          case other => List(other)
        }
        PolarAnd(flat, !polarity)
      case And(children) =>
        val polarized = children.map(oldPolarize(_, true))
        val flat = polarized.flatMap {
          case PolarAnd(ch, true) => ch
          case other => List(other)
        }
        PolarAnd(flat, polarity)
      case FunApplication(sym, args) => PolarFunApplication(sym, args.map(oldPolarize(_, true)), polarity)
      case Literal(b) => PolarLiteral(b == polarity)
    }
    if polarity then f.oldPolarFormula = Some(r)
    else f.oldPolarFormula = Some(getInversePolar(r))
    r
  }

  override def isSame(formula1: Formula, formula2: Formula): Boolean =
    checkEquivalence(oldPolarize(formula1), oldPolarize(formula2))

  def isOLSmaller(formula1: Formula, formula2: Formula): Boolean =
    latticesLEQ(nPnormalForm(oldPolarize(formula1)), nPnormalForm(oldPolarize(formula2)))

  override def reducedForm(formula: Formula): Formula =
    val p = oldPolarize(formula)
    val nf = nPnormalForm(p)
    val res = toFormula(nf)
    res
}