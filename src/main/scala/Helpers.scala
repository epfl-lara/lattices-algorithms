
package ortholattices
import ortholattices.algorithms.*

object Helpers {
  export Datastructures.*

  def bot: Formula = Literal(false)
  def top: Formula = Literal(true)
  def neg(f: Formula): Neg = Neg(f)
  def and(args: List[Formula]): And = And(args)
  def and(f:Formula*): And = And(f.toList)
  def or(args: List[Formula]): Or = Or(args)
  def or(f:Formula*): Or = Or(f.toList)
  def iff(f: Formula, g: Formula): And = and(implies(f, g), implies(g, f))
  def implies(f: Formula, g: Formula): Formula = neg(or(neg(f), g))


  val OLAlgo = new OLAlgorithm
  val OcbslAlgo = new OcbslAlgorithm

  def ¬(f: Formula): Neg = Neg(f)
  extension (f: Formula) {
    def &(g: Formula): And = and(f, g)
    def /\(g: Formula): And = and(f, g)
    def |(g: Formula): Or = or(f, g)
    def \/(g: Formula): Or = or(f, g)
    def unary_! : Neg = Neg(f)

    def <->(g: Formula): And = iff(f, g)
    def -->(g: Formula): Formula = implies(f, g)


    def OLnormalize: Formula = OLAlgo.reducedForm(f)
    def OcbslNormalize: Formula = OcbslAlgo.reducedForm(f)
    infix def ~(g: Formula): Boolean = OLAlgo.isSame(f, g)
  }

  def ⊥ : Formula = bot
  def ⊤ : Formula = top

  given conv_false: Conversion[false, Formula] = _ => bot
  given conv_true: Conversion[true, Formula] = _ => top

  val x0 = Variable(0)
  val x1 = Variable(1)
  val x2 = Variable(2)
  val x3 = Variable(3)
  val x4 = Variable(4)
  val x5 = Variable(5)
  val x6 = Variable(6)
  val x7 = Variable(7)
  val x8 = Variable(8)
  val x9 = Variable(9)
  val x10 = Variable(10)
  val x11 = Variable(11)
  val x12 = Variable(12)
  val x13 = Variable(13)
  val x14 = Variable(14)
  val x15 = Variable(15)
  val x16 = Variable(16)
  val x17 = Variable(17)
  val x18 = Variable(18)
  val x19 = Variable(19)
  val x20 = Variable(20)
  val x21 = Variable(21)
  val x22 = Variable(22)
  val x23 = Variable(23)
  val x24 = Variable(24)
  val x25 = Variable(25)
  val x26 = Variable(26)
  val x27 = Variable(27)
  val x28 = Variable(28)
  val x29 = Variable(29)
  val x30 = Variable(30)

}
