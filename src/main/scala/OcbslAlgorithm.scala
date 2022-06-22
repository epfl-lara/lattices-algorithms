import Datastructures.*

import scala.collection.mutable
import scala.math.Numeric.IntIsIntegral

/**
 * An EquivalenceChecker is an object that allows to detect equivalence between formulas in the
 * theory of Orthocomplemented Bisemilattices.
 * This allows proof checkers and writers to avoid having to deal with a class of "easy" equivalence.
 * For example, by considering "x ∨ y" as being the same formula as "y ∨ x", we can avoid frustrating errors.
 * This relation is always a subrelation of the usual FOL implication.
 */
object OcbslAlgorithm extends EquivalenceAndNormalForAlgorithm {
  sealed abstract class NoAndFormula {
    val size: Int
    var ocbslNormalForm: Option[NormalFormula] = None
    override def toString: String = Printer.pretty(this)
  }
  case class NoAndVariable(id: Int) extends NoAndFormula {
    val size = 1
  }
  case class NoAndNeg(child: NoAndFormula) extends NoAndFormula {
    val size: Int = 1 + child.size
  }
  case class NoAndOr(children: List[NoAndFormula]) extends NoAndFormula {
    val size: Int = (children map (_.size)).foldLeft(1) { case (a, b) => a + b }
  }
  case class NoAndLiteral(b: Boolean) extends NoAndFormula {
    val size = 1
  }

  sealed abstract class NormalFormula {
    val code: Int
    override def toString: String = Printer.pretty(this)
  }
  case class NVariable(id: Int, code: Int) extends NormalFormula
  case class NNeg(child: NormalFormula, code: Int) extends NormalFormula
  case class NOr(children: List[NormalFormula], code: Int) extends NormalFormula
  case class NLiteral(b: Boolean) extends NormalFormula {
    val code: Int = if (b) 1 else 0
  }

  /**
   * Puts back in regular formula syntax, and performs negation normal form to produce shorter version.
   */
  def toFormula(f: NormalFormula, positive: Boolean = true): Formula = f match
    case NVariable(id, code) => if (positive) Variable(id) else Neg(Variable(id))
    case NNeg(child, code) => toFormula(child, !positive)
    case NOr(children, code) => if positive then Or(children.map(c => toFormula(c, true))) else And(children.map(c => toFormula(c, false)))
    case NLiteral(b) => Literal(positive == b)

  private val codesSig: mutable.HashMap[(String, Seq[Int]), Int] = mutable.HashMap()
  codesSig.update(("zero", Nil), 0)
  codesSig.update(("one", Nil), 1)

  def hasNormaleRecComputed(sf: NoAndFormula): Boolean = sf.ocbslNormalForm.nonEmpty && (sf match {
    case NoAndNeg(child) => hasNormaleRecComputed(child)
    case NoAndOr(children) => children.forall(c => hasNormaleRecComputed(c))
    case _ => true
  })

  def checkForContradiction(children: List[(NormalFormula, Int)]): Boolean = {
    val (negatives_temp, positives_temp) = children.foldLeft[(List[NormalFormula], List[NormalFormula])]((Nil, Nil))((acc, ch) =>
      acc match {
        case (negatives, positives) =>
          ch._1 match {
            case NNeg(child, c) => (child :: negatives, positives)
            case _ => (negatives, ch._1 :: positives)
          }
      }
    )
    val negatives = negatives_temp.sortBy(_.code)
    val positives = positives_temp.reverse
    var i, j = 0
    while (i < positives.size && j < negatives.size) { // checks if there is a positive and negative nodes with same code.
      val (c1, c2) = (positives(i).code, negatives(j).code)
      if (c1 < c2) i += 1
      else if (c1 == c2)
        return true
      else j += 1
    }

    val children_codes = children.map(c => c._2).toSet
    var k = 0
    while (k < negatives.size) {
      negatives(k) match {
        case NOr(gdChildren, c) =>
          if (gdChildren.forall(sf => children_codes.contains(sf.code))) return true
        case _ => ()
      }
      k += 1
    }
    false
  }

  def updateCodesSig(sig: (String, Seq[Int])): Int = {
    if (!codesSig.contains(sig)) codesSig.update(sig, codesSig.size)
    codesSig(sig)
  }

  /**
   * Assumes the formula to be free of And. Apply removeAnd first.
   */
  def OCBSLCode(phi: NoAndFormula): Int = {
    if (phi.ocbslNormalForm.nonEmpty) return phi.ocbslNormalForm.get.code
    val L = pDisj(phi, Nil)
    val L2 = L zip (L map (_.code))
    val L3 = L2.sortBy(_._2).distinctBy(_._2).filterNot(_._2 == 0) // not most efficient on sorted list but no big deal for now
    if (L3.isEmpty) {
      phi.ocbslNormalForm = Some(NLiteral(false))
    } else if (L3.length == 1) {
      phi.ocbslNormalForm = Some(L3.head._1)
    } else if (L3.exists(_._2 == 1) || checkForContradiction(L3)) {
      phi.ocbslNormalForm = Some(NLiteral(true))
    } else {
      phi.ocbslNormalForm = Some(NOr(L3.map(_._1), updateCodesSig(("or", L3.map(_._2)))))
    }
    phi.ocbslNormalForm.get.code
  }

  def pDisj(phi: NoAndFormula, acc: List[NormalFormula]): List[NormalFormula] = {
    if (phi.ocbslNormalForm.nonEmpty) return pDisjNormal(phi.ocbslNormalForm.get, acc)
    val r: List[NormalFormula] = phi match {
      case NoAndVariable(id) =>
        val lab = "pred_" + id
        phi.ocbslNormalForm = Some(NVariable(id, updateCodesSig((lab, Nil))))
        phi.ocbslNormalForm.get :: acc
      case NoAndNeg(child) => pNeg(child, phi, acc)
      case NoAndOr(children) => children.foldLeft(acc)((p, a) => pDisj(a, p))
      case NoAndLiteral(true) =>
        phi.ocbslNormalForm = Some(NLiteral(true))
        phi.ocbslNormalForm.get :: acc
      case NoAndLiteral(false) =>
        phi.ocbslNormalForm = Some(NLiteral(false))
        phi.ocbslNormalForm.get :: acc
    }
    r
  }

  def pNeg(phi: NoAndFormula, parent: NoAndFormula, acc: List[NormalFormula]): List[NormalFormula] = {
    if (phi.ocbslNormalForm.nonEmpty) return pNegNormal(phi.ocbslNormalForm.get, parent, acc)
    val r: List[NormalFormula] = phi match {
      case NoAndVariable(id) =>
        val lab = "pred_" + id
        phi.ocbslNormalForm = Some(NVariable(id, updateCodesSig((lab, Nil))))
        parent.ocbslNormalForm = Some(NNeg(phi.ocbslNormalForm.get, updateCodesSig(("neg", List(phi.ocbslNormalForm.get.code)))))
        parent.ocbslNormalForm.get :: acc
      case NoAndNeg(child) => pDisj(child, acc)
      case NoAndLiteral(true) =>
        parent.ocbslNormalForm = Some(NLiteral(false))
        parent.ocbslNormalForm.get :: acc
      case NoAndLiteral(false) =>
        parent.ocbslNormalForm = Some(NLiteral(true))
        parent.ocbslNormalForm.get :: acc
      case NoAndOr(children) =>
        val T = children.sortBy(_.size)
        val r1 = T.tail.foldLeft(List[NormalFormula]())((p, a) => pDisj(a, p))
        val r2 = r1 zip (r1 map (_.code))
        val r3 = r2.sortBy(_._2).distinctBy(_._2).filterNot(_._2 == 0)
        if (r3.isEmpty) pNeg(T.head, parent, acc)
        else {
          val s1 = pDisj(T.head, r1)
          val s2 = s1 zip (s1 map (_.code))
          val s3 = s2.sortBy(_._2).distinctBy(_._2).filterNot(_._2 == 0)
          if (s3.exists(_._2 == 1) || checkForContradiction(s3)) {
            phi.ocbslNormalForm = Some(NLiteral(true))
            parent.ocbslNormalForm = Some(NLiteral(false))
            parent.ocbslNormalForm.get :: acc
          } else if (s3.length == 1) {
            pNegNormal(s3.head._1, parent, acc)
          } else {
            phi.ocbslNormalForm = Some(NOr(s3.map(_._1), updateCodesSig(("or", s3.map(_._2)))))
            parent.ocbslNormalForm = Some(NNeg(phi.ocbslNormalForm.get, updateCodesSig(("neg", List(phi.ocbslNormalForm.get.code)))))
            parent.ocbslNormalForm.get :: acc
          }
        }
    }
    r
  }

  def pDisjNormal(f: NormalFormula, acc: List[NormalFormula]): List[NormalFormula] = f match {
    case NOr(children, c) => children ++ acc
    case _ => f :: acc
  }

  def pNegNormal(f: NormalFormula, parent: NoAndFormula, acc: List[NormalFormula]): List[NormalFormula] = f match {
    case NNeg(child, c) =>
      pDisjNormal(child, acc)
    case _ =>
      parent.ocbslNormalForm = Some(NNeg(f, updateCodesSig(("neg", List(f.code)))))
      parent.ocbslNormalForm.get :: acc
  }

  def checkEquivalence(formula1: NoAndFormula, formula2: NoAndFormula): Boolean = {
    getCode(formula1) == getCode(formula2)
  }

  def getCode(formula: NoAndFormula): Int = OCBSLCode(formula)

  override def isSame(formula1: Formula, formula2: Formula): Boolean = checkEquivalence(removeAnd(formula1), removeAnd(formula2))

  override def reducedForm(formula: Formula): Formula = {
    val f = removeAnd(formula)
    getCode(f)
    toFormula(f.ocbslNormalForm.get)
  }

  def removeAnd(f: Formula, positive: Boolean = true): NoAndFormula = f match {
    case Variable(id) => if positive then NoAndVariable(id) else NoAndNeg(NoAndVariable(id))
    case Neg(child) => removeAnd(child, !positive)
    case Or(children) =>
      if positive then NoAndOr(children.map(c => removeAnd(c, true))) else NoAndNeg(NoAndOr(children.map(c => removeAnd(c, true))))
    case And(children) =>
      if positive then NoAndNeg(NoAndOr(children.map(c => removeAnd(c, false)))) else NoAndOr(children.map(c => removeAnd(c, false)))
    case Literal(b) => NoAndLiteral(b == positive)
  }

}
