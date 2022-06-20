import Datastructures._
object Main {

    def main(args: Array[String]):Unit = {

        val positiveTestCases: List[(Formula, Formula)] = List(
            and(a, b) -> and(b, a),
            or(a, b) -> or(b, a),
            iff(a, b) -> iff(b, a),
            and(and(a, b), c) -> and(a, and(b, c)),
            or(or(a, b), c) -> or(a, or(b, c)),
            neg(a) -> neg(neg(neg(a))),
            a -> neg(neg(neg(neg(a)))),
            and(a, b) -> neg(or(neg(a), neg(b))),
            or(a, b) -> neg(and(neg(a), neg(b))),
            or(a, neg(a)) -> or(neg(a), neg(neg(a))),
            and(a, neg(a)) -> and(neg(a), neg(neg(a))),
            and(a, a) -> a,
            or(a, a) -> a
        )

        val negativeTestCases: List[(Formula, Formula)] = List(
            a -> implies(a, x),
            a -> iff(a, x),
            and(a, or(x, y)) -> or(and(a, x), and(a, y)),
            or(a, and(x, y)) -> and(or(a, x), or(a, y)),
        )
        println("Positive Test Cases")
        positiveTestCases.foreach{ case (f,g) => println((new OcbslAlgorithm).isSame(f,g))}
        println("Negative Test Cases")
        negativeTestCases.foreach{ case (f,g) => println((new OcbslAlgorithm).isSame(f,g))}

    }

    val a = Variable("a")
    val b = Variable("b")
    val c = Variable("c")
    val x = Variable("x")
    val y = Variable("y")
    def neg(f:Formula):Formula = Neg(f)
    def and(args:List[Formula]): Formula = And(args)
    def and(f:Formula, g:Formula): Formula = And(List(f,g))
    def or(args:List[Formula]): Formula = Or(args)
    def or(f:Formula, g:Formula): Formula = Or(List(f,g))
    def iff(f:Formula, g:Formula): Formula = and(implies(f,g), implies(g,f))
    def implies(f:Formula, g:Formula): Formula = neg(or(neg(f),g))
}
