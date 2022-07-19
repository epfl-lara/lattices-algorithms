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
class OcbslAlgorithm extends EquivalenceAndNormalFormAlgorithm {
  import OcbslAlgorithm.*

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
        case NAnd(gdChildren, c) =>
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
   * Assumes the formula to be free of And. Apply removeOr first.
   */
  def OCBSLCode(phi: NoOrFormula): Int = {
    if (phi.ocbslNormalForm.nonEmpty) return phi.ocbslNormalForm.get.code
    val L = pDisj(phi, Nil)
    val L2 = L zip (L map (_.code))
    val L3 = L2.sortBy(_._2).distinctBy(_._2).filterNot(_._2 == 1) // not most efficient on sorted list but no big deal for now
    if (L3.isEmpty) {
      phi.ocbslNormalForm = Some(NLiteral(true))
    } else if (L3.length == 1) {
      phi.ocbslNormalForm = Some(L3.head._1)
    } else if (L3.exists(_._2 == 0) || checkForContradiction(L3)) {
      phi.ocbslNormalForm = Some(NLiteral(false))
    } else {
      phi.ocbslNormalForm = Some(NAnd(L3.map(_._1), updateCodesSig(("or", L3.map(_._2)))))
    }
    phi.ocbslNormalForm.get.code
  }

  def pDisj(phi: NoOrFormula, acc: List[NormalFormula]): List[NormalFormula] = {
    if (phi.ocbslNormalForm.nonEmpty){
      return pDisjNormal(phi.ocbslNormalForm.get, acc)
    }
    val r: List[NormalFormula] = phi match {
      case NoOrVariable(id) =>
        val lab = "pred_" + id
        phi.ocbslNormalForm = Some(NVariable(id, updateCodesSig((lab, Nil))))
        phi.ocbslNormalForm.get :: acc
      case NoOrNeg(child) => pNeg(child, phi, acc)
      case NoOrAnd(children) => children.foldLeft(acc)((p, a) => pDisj(a, p))
      case NoOrLiteral(true) =>
        phi.ocbslNormalForm = Some(NLiteral(true))
        phi.ocbslNormalForm.get :: acc
      case NoOrLiteral(false) =>
        phi.ocbslNormalForm = Some(NLiteral(false))
        phi.ocbslNormalForm.get :: acc
    }
    r
  }

  def pNeg(phi: NoOrFormula, parent: NoOrFormula, acc: List[NormalFormula]): List[NormalFormula] = {
    if (phi.ocbslNormalForm.nonEmpty){
      return pNegNormal(phi.ocbslNormalForm.get, parent, acc)
    }
    val r: List[NormalFormula] = phi match {
      case NoOrVariable(id) =>
        val lab = "pred_" + id
        phi.ocbslNormalForm = Some(NVariable(id, updateCodesSig((lab, Nil))))
        parent.ocbslNormalForm = Some(NNeg(phi.ocbslNormalForm.get, updateCodesSig(("neg", List(phi.ocbslNormalForm.get.code)))))
        parent.ocbslNormalForm.get :: acc
      case NoOrNeg(child) => pDisj(child, acc)
      case NoOrLiteral(true) =>
        parent.ocbslNormalForm = Some(NLiteral(false))
        parent.ocbslNormalForm.get :: acc
      case NoOrLiteral(false) =>
        parent.ocbslNormalForm = Some(NLiteral(true))
        parent.ocbslNormalForm.get :: acc
      case NoOrAnd(children) =>
        val T = children.sortBy(_.size)
        val r1 = T.tail.foldLeft(List[NormalFormula]())((p, a) => pDisj(a, p))
        val r2 = r1 zip (r1 map (_.code))
        val r3 = r2.sortBy(_._2).distinctBy(_._2).filterNot(_._2 == 1)
        if (r3.isEmpty) pNeg(T.head, parent, acc)
        else {
          val s1 = pDisj(T.head, r1)
          val s2 = s1 zip (s1 map (_.code))
          val s3 = s2.sortBy(_._2).distinctBy(_._2).filterNot(_._2 == 1)
          if (s3.exists(_._2 == 0) || checkForContradiction(s3)) {
            phi.ocbslNormalForm = Some(NLiteral(false))
            parent.ocbslNormalForm = Some(NLiteral(true))
            parent.ocbslNormalForm.get :: acc
          } else if (s3.length == 1) {
            pNegNormal(s3.head._1, parent, acc)
          } else {
            phi.ocbslNormalForm = Some(NAnd(s3.map(_._1), updateCodesSig(("or", s3.map(_._2)))))
            parent.ocbslNormalForm = Some(NNeg(phi.ocbslNormalForm.get, updateCodesSig(("neg", List(phi.ocbslNormalForm.get.code)))))
            parent.ocbslNormalForm.get :: acc
          }
        }
    }
    r
  }

  def pDisjNormal(f: NormalFormula, acc: List[NormalFormula]): List[NormalFormula] = f match {
    case NAnd(children, c) => children ++ acc
    case _ => f :: acc
  }

  def pNegNormal(f: NormalFormula, parent: NoOrFormula, acc: List[NormalFormula]): List[NormalFormula] = f match {
    case NNeg(child, c) =>
      pDisjNormal(child, acc)
    case _ =>
      parent.ocbslNormalForm = Some(NNeg(f, updateCodesSig(("neg", List(f.code)))))
      parent.ocbslNormalForm.get :: acc
  }

  def checkEquivalence(formula1: NoOrFormula, formula2: NoOrFormula): Boolean = {
    getCode(formula1) == getCode(formula2)
  }

  def getCode(formula: NoOrFormula): Int = OCBSLCode(formula)

  override def isSame(formula1: Formula, formula2: Formula): Boolean = checkEquivalence(removeOr(formula1), removeOr(formula2))

  override def reducedForm(formula: Formula): Formula = {
    val f = removeOr(formula)
    getCode(f)
    val r = toFormula(f.ocbslNormalForm.get)
    r
  }
}

object OcbslAlgorithm extends EquivalenceAndNormalFormAlgorithm {

  private val codesSig: mutable.HashMap[(String, Seq[Int]), Int] = mutable.HashMap()
  codesSig.update(("zero", Nil), 0)
  codesSig.update(("one", Nil), 1)

  var totNoOrFormula:Int = 0
  sealed abstract class NoOrFormula {
    val size: BigInt
    val uniqueKey: Int = totNoOrFormula
    var ocbslNormalForm: Option[NormalFormula] = None
    override def toString: String = Printer.pretty(this)
    totNoOrFormula+=1
  }
  case class NoOrVariable(id: Int) extends NoOrFormula {
    val size = 1
  }
  case class NoOrNeg(child: NoOrFormula) extends NoOrFormula {
    val size = child.size + (1:BigInt)
  }
  case class NoOrAnd(children: List[NoOrFormula]) extends NoOrFormula {
    val size = (children map (_.size)).foldLeft(1:BigInt) { case (a, b) => a + b }
  }
  case class NoOrLiteral(b: Boolean) extends NoOrFormula {
    val size = 1
  }

  var totNormalFormula = 0
  sealed abstract class NormalFormula {
    val code: Int
    val uniqueKey: Int = totNormalFormula
    var formulaP: Option[Formula] = None
    var formulaN: Option[Formula] = None
    var formulaAIG: Option[Formula] = None
    override def toString: String = Printer.pretty(this)
    totNormalFormula+=1
  }
  case class NVariable(id: Int, code: Int) extends NormalFormula
  case class NNeg(child: NormalFormula, code: Int) extends NormalFormula
  case class NAnd(children: List[NormalFormula], code: Int) extends NormalFormula
  case class NLiteral(b: Boolean) extends NormalFormula {
    val code: Int = if (b) 1 else 0
  }

  def toFormula(formula: NormalFormula):Formula = toFormulaAIG(formula)
  /**
   * Puts back in regular formula syntax, and performs negation normal form to produce shorter version.
   */
  def toFormulaNNF(f: NormalFormula, positive: Boolean = true): Formula =
    if positive & f.formulaP.isDefined then return f.formulaP.get
    if !positive & f.formulaN.isDefined then return f.formulaN.get
    val r = f match
      case NVariable(id, code) => if (positive) Variable(id) else Neg(toFormulaNNF(f, true))
      case NNeg(child, code) => toFormulaNNF(child, !positive)
      case NAnd(children, code) => if positive then And(children.map(c => toFormulaNNF(c, true))) else Or(children.map(c => toFormulaNNF(c, false)))
      case NLiteral(b) => Literal(positive == b)
    if positive then f.formulaP = Some(r)
    else  f.formulaN = Some(r)
    r

  def toFormulaAIG(f: NormalFormula): Formula =
    if f.formulaAIG.isDefined then return f.formulaAIG.get
    val r = f match
      case NVariable(id, code) => Variable(id)
      case NNeg(child, code) => Neg(toFormulaAIG(child))
      case NAnd(children, code) => And(children.map(c => toFormulaAIG(c)))
      case NLiteral(b) => Literal(b)
    f.formulaAIG = Some(r)
    r


  def checkEquivalence(formula1: NoOrFormula, formula2: NoOrFormula): Boolean = (new OcbslAlgorithm).checkEquivalence(formula1, formula2)

  def getCode(formula: NoOrFormula): Int = (new OcbslAlgorithm).getCode(formula)

  override def isSame(formula1: Formula, formula2: Formula): Boolean = (new OcbslAlgorithm).isSame(formula1, formula2)

  override def reducedForm(formula: Formula): Formula = (new OcbslAlgorithm).reducedForm(formula)


  def removeOr(f: Formula, positive: Boolean = true): NoOrFormula =
    if positive & f.noOrFormulaP.isDefined then return f.noOrFormulaP.get
    if !positive & f.noOrFormulaN.isDefined then return f.noOrFormulaN.get
    val r = f match {
      case Variable(id) => if positive then NoOrVariable(id) else NoOrNeg(removeOr(f, true))
      case Neg(child) => removeOr(child, !positive)
      case Or(children) =>
        if positive then NoOrNeg(removeOr(f, false)) else NoOrAnd(children.map(c => removeOr(c, false)))
      case And(children) =>
        if positive then NoOrAnd(children.map(c => removeOr(c, true))) else NoOrNeg(removeOr(f, true))
      case Literal(b) => NoOrLiteral(b == positive)
    }
    if positive then f.noOrFormulaP = Some(r)
    else f.noOrFormulaN = Some(r)
    r

}
