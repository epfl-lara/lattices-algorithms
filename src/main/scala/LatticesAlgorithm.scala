import Datastructures.*

class LatticesAlgorithm extends EquivalenceAndNormalForAlgorithm{

  def negationNormalForm(f:Formula, positive:Boolean=true):Formula = f match {
    case Variable(id) => if (positive) f else Neg(f)
    case Neg(child) => negationNormalForm(child, !positive)
    case Or(children) => if positive then Or(children.map(c => negationNormalForm(c, positive))) else And(children.map(c => negationNormalForm(c, positive)))
    case And(children) => if positive then And(children.map(c => negationNormalForm(c, positive))) else Or(children.map(c => negationNormalForm(c, positive)))
    case Literal(b) => Literal(positive == b)
  }

  def beta(f:Formula):Formula = f match
    case _: (Variable | Neg | Literal) => f
    case Or(children) =>
      val r = Or(children map beta)
      if isReduced(r) then r else Literal(true)
    case And(children) =>
      val r = And(children map beta)
      if isReduced(r) then r else Literal(false)

  def isReduced(f: Formula): Boolean = f match
    case _: (Variable | Neg | Literal) => true
    case And(children) => children.forall(c => isReduced(c) & latticesLEQ(f, negationNormalForm(c, false)))
    case Or(children) => children.forall(c => isReduced(c) & latticesLEQ(negationNormalForm(c, false), f))


  /*
  Assumes the formula to be in negation normal form. Computes order wrt (non orthocomplemented) lattices.
  */
  def latticesLEQ(formula1: Formula, formula2: Formula): Boolean = {
    type Lit = (Variable | Neg)
    (formula1, formula2) match {
      case (Literal(b), _) => !b
      case (_, Literal(b)) => b
      case (v1: Lit, v2: Lit) => v1 == v2
      case (_, Or(children)) => children.forall(c => latticesLEQ(formula1, c))
      case (And(children), _) => children.forall(c => latticesLEQ(c, formula2))
      case (v: Lit, And(children)) => children.exists(c => latticesLEQ(v, c))
      case (Or(children), v: Lit) => children.exists(c => latticesLEQ(c, v))
      case (Or(children1), And(children2)) => children1.exists(c => latticesLEQ(c, formula2)) || children2.exists(c => latticesLEQ(formula2, c))
      //case _ => false
    }
  }


  override def isSame(formula1: Formula, formula2: Formula): Boolean = ???

  override def normalForm(formula: Formula): NormalFormula = ???


}
