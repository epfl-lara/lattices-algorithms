import Datastructures.*

import scala.collection.mutable

object LatticesAlgorithm extends EquivalenceAndNormalForAlgorithm {

  sealed abstract class NNFFormula {
    override def equals(obj: Any): Boolean = obj match
      case o: AnyRef => eq(o)
      case _ => super.equals(obj)
    val size: Int

    override def toString: String = Printer.pretty(this)
    // memoize the negation in a formula. Gain a factor n in space and time when
    // computing the nnf of all subformulas of a subformula compared to a functional approach.
    var inverse: Option[NNFFormula] = None
    var reducedForm: Option[NNFFormula] = None
    var isReduced: Option[Boolean] = None
    var lessThan: mutable.HashMap[NNFFormula, Boolean] = mutable.HashMap(this -> true)

  }
  case class NNFVariable(id: Int, polarity: Boolean) extends NNFFormula {
    val size = 1
  }
  case class NNFOr(children: List[NNFFormula]) extends NNFFormula {
    val size: Int = (children map (_.size)).foldLeft(1) { case (a, b) => a + b }
  }
  case class NNFAnd(children: List[NNFFormula]) extends NNFFormula {
    val size: Int = (children map (_.size)).foldLeft(1) { case (a, b) => a + b }
  }
  case class NNFLiteral(b: Boolean) extends NNFFormula {
    val size = 1
  }

  /**
   * Goes back to original representation
   */
  def toFormula(f: NNFFormula): Formula = f match
    case NNFVariable(id, polarity) => if polarity then Variable(id) else Neg(Variable(id))
    case NNFOr(children) => Or(children map (toFormula))
    case NNFAnd(children) => And(children map (toFormula))
    case NNFLiteral(b) => Literal(b)

  /**
   * compute and memoize the negation of a formula and all its subformula, and recursively for the negated version
   */
  def getInverse(f: NNFFormula): NNFFormula = {
    f.inverse match
      case Some(value) => value
      case None =>
        val second = f match
          case NNFVariable(id, polarity) => NNFVariable(id, !polarity)
          case NNFOr(children) => NNFAnd(children.map(getInverse))
          case NNFAnd(children) => NNFOr(children.map(getInverse))
          case NNFLiteral(b) => NNFLiteral(!b)
        f.inverse = Some(second)
        second.inverse = Some(f)
        second
  }

  /**
   * Transforms the regular Formula a representation into the one specific to the OL algorithm.
   */
  def negationNormalForm(f: Formula, positive: Boolean = true): NNFFormula = f match {
    case Variable(id) => if (positive) NNFVariable(id, true) else NNFVariable(id, false)
    case Neg(child) => negationNormalForm(child, !positive)
    case Or(children) => if positive then NNFOr(children.map(c => negationNormalForm(c, true))) else NNFAnd(children.map(c => negationNormalForm(c, false)))
    case And(children) => if positive then NNFAnd(children.map(c => negationNormalForm(c, true))) else NNFOr(children.map(c => negationNormalForm(c, false)))
    case Literal(b) => NNFLiteral(positive == b)
  }

  /**
   * In the end, it beta has been called on the whole formula: N calls to beta
   * isReduced has been called on all subformula OF THE RESULT (plus those that disappeared in later beta calls),
   * and on none of the original formulas. So isReduced is called on a set of n (sub)formulas (plus inverses)
   * So quadratic time.
   */
  def beta(f: NNFFormula): NNFFormula = {
    f.reducedForm match
      case Some(value) => value
      case None =>
        val a = f match
          case _: (NNFVariable | NNFLiteral) => f
          case NNFOr(children) =>
            val r = NNFOr(children map beta)
            if isReduced(r) then r
            else NNFLiteral(true)
          case NNFAnd(children) =>
            val r = NNFAnd(children map beta)
            if isReduced(r) then r else NNFLiteral(false)
        f.reducedForm = Some(a)
        a
  }

  /**
   * Quadratic time: for formula f of size n, we only create  f' of size n.
   * Potential number of pair of formulas: n**2
   * So all calls to latticesLEQ take at most time n**2
   * Recusrion is n steps, each of constant time (not counting latticesLEQ)
   * Hence time quadratic, and computes the function for all children and inverses along the way:
   * Quadratic time to compute the value on all subformulas of a formula.
   * if f == NNFAnd(children) and children have been beta-computed:
   * for each children, the procedure takes time O(1) + T(latticesLEQ(f, getInverse(c)))
   */
  def isReduced(f: NNFFormula): Boolean = {
    f.isReduced match
      case Some(value) => value
      case None =>
        val a = f match
          case _: (NNFVariable | NNFLiteral) => true
          case NNFOr(children) => children.forall(c => isReduced(c) & !latticesLEQ(getInverse(c), f))
          case NNFAnd(children) => children.forall(c => isReduced(c) & !latticesLEQ(f, getInverse(c)))
        f.isReduced = Some(a)
        a
  }

  /**
   * Computes order wrt (non orthocomplemented) lattices.
   * Computing all pairs of (sub)formulas of two large formulas take time at most n**2
   */
  def latticesLEQ(formula1: NNFFormula, formula2: NNFFormula): Boolean = {
    formula1.lessThan.get(formula2) match
      case Some(value) => value
      case None =>
        val r = (formula1, formula2) match {
          case (NNFLiteral(b1), NNFLiteral(b2)) => !b1 || b2
          case (NNFLiteral(b), _) => !b
          case (_, NNFLiteral(b)) => b
          case (NNFVariable(id1, polarity1), NNFVariable(id2, polarity2)) =>
            id1 == id2 && polarity1 == polarity2
          case (_, NNFAnd(children)) =>
            children.forall(c => latticesLEQ(formula1, c))
          case (NNFOr(children), _) =>
            children.forall(c => latticesLEQ(c, formula2))
          case (v: NNFVariable, NNFOr(children)) =>
            children.exists(c => latticesLEQ(v, c))
          case (NNFAnd(children), v: NNFVariable) =>
            children.exists(c => latticesLEQ(c, v))
          case (NNFAnd(children1), NNFOr(children2)) =>
            children1.exists(c => latticesLEQ(c, formula2)) || children2.exists(c => latticesLEQ(formula1, c))
        }
        formula1.lessThan.update(formula2, r)
        r
  }

  override def isSame(formula1: Formula, formula2: Formula): Boolean =
    val a = nNFnormalForm(formula1)
    val b = nNFnormalForm(formula2)
    latticesLEQ(a, b) & latticesLEQ(b, a)

  def nNFnormalForm(formula: Formula): NNFFormula = {
    beta(negationNormalForm(formula))
  }

  override def reducedForm(formula: Formula): Formula = toFormula(nNFnormalForm(formula))

  def olLEQ(formula1: Formula, formula2: Formula): Boolean = {
    latticesLEQ(nNFnormalForm(formula1), nNFnormalForm(formula2))
  }

}
