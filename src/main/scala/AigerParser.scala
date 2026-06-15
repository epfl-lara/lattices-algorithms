package ortholattices
import java.io.{BufferedInputStream, FileInputStream}
import ortholattices.algorithms.Datastructures.*
import scala.collection.mutable

object AigerParser {

  def getAigerFormulas(path:String): List[Formula] = getAigerFormulasWithInfo(path)._1

  /** Like getAigerFormulas but also returns the number of primary inputs I,
   *  so callers know the first free variable id (= 2*(I+1)). */
  def getAigerFormulasWithInfo(path: String): (List[Formula], Int) = {
    val bis = new BufferedInputStream(new FileInputStream(path))
    val header:Header = lineToHeader(bis.readLine())
    if header.L != 0 then throw new Exception("Only combinatorial circuits without latches are supported. L must be equal to 0")
    var outputs : List[Int] = Nil

    for (i <- 1 to header.O) {
      val l = bis.readLine()
      outputs = l.toInt :: outputs
    }

    val table:mutable.HashMap[Int, Formula] = mutable.HashMap()
    table.update(0, Literal(false))
    table.update(1, Literal(true))

    for (i <- 1 to header.I){
      val v = Variable(2*i)
      table.update(2*i, v)
      table.update(2*i+1, Neg(v))
    }

    for (i <- header.I+1 to header.M){
      val (rh0, rh1) = bis.readAndGate(2*i)
      val a = And(List(table(rh0), table(rh1)))
      table.update(2*i, a)
      table.update(2*i+1, Neg(a))
    }
    (outputs map table, header.I)
  }

  /**
   * Tseitin encoding of a list of formulas as per the tex.
   *
   * For each non-atom subformula (And or Or node) encountered across all
   * formulas, assigns a fresh Variable starting from startVar and adds two
   * axioms per the tex encoding:
   *   x ≤ φ   (Variable(x), φ_shallow)
   *   φ ≤ x   (φ_shallow, Variable(x))
   * where φ_shallow is the shallow form using the fresh names of direct children.
   *
   * Neg is NOT given a fresh variable: Neg(compound) becomes Neg(childName),
   * which is already an atom (childName is a Variable after Tseitin-renaming).
   *
   * Shared subformulas — same Formula object — get the same fresh variable.
   * This preserves DAG structure and is correct when formulas share sub-trees.
   *
   * startVar must not overlap any Variable id already in the formulas.
   * Returns (rootNames, axioms, nextFreeVar) where nextFreeVar is the first
   * variable id not used by this encoding; pass it as startVar to a second
   * independent call to produce a disjoint copy.
   */
  def tseitinAxioms(formulas: Seq[Formula], startVar: Int)
      : (Seq[Formula], Set[(Formula, Formula)], Int) = {
    val axioms = mutable.HashSet[(Formula, Formula)]()
    var nextVar = startVar
    // Memoize by object identity: same Formula object → same Tseitin name.
    val memo = new java.util.IdentityHashMap[Formula, Formula]()

    def go(f: Formula): Formula = {
      val cached = memo.get(f)
      if cached != null then return cached
      val result: Formula = f match
        case _: Variable | _: Literal =>
          f
        case Neg(inner) =>
          val innerName = go(inner)
          if innerName eq inner then f else Neg(innerName)
        case And(cs) =>
          val childNames = cs.map(go)
          val phi = And(childNames)
          val x = Variable(nextVar); nextVar += 2
          axioms += ((x, phi))
          axioms += ((phi, x))
          x
        case Or(cs) =>
          val childNames = cs.map(go)
          val phi = Or(childNames)
          val x = Variable(nextVar); nextVar += 2
          axioms += ((x, phi))
          axioms += ((phi, x))
          x
        case other => other
      memo.put(f, result)
      result
    }

    val rootNames = formulas.map(go)
    (rootNames, axioms.toSet, nextVar)
  }



  def lineToHeader(line:String): Header = {
    val s"aig $m $i $l $o $a" = line
    Header(m.toInt, i.toInt, l.toInt, o.toInt, a.toInt)
  }
  case class Header(M:Int, I:Int, L:Int, O:Int, A:Int)

  /**
   * Extract a single-output sub-circuit from a binary AIGER file and write it
   * as ASCII AIGER (AAG).
   *
   * Only AND gates in the transitive fanin of the chosen output are included.
   * Gates are re-numbered contiguously; inputs (1..nInputs) are preserved.
   * The result can be passed directly to `olsc --entail` without any NNF
   * conversion in Scala — the C binary's read_nnf_formula handles AIGER
   * correctly (AND gates with literal-encoded polarities, as in the original).
   */
  def writeOutputSubcircuit(sourcePath: String, outputIndex: Int, destPath: String): Unit =
    import java.nio.file.{Files, Paths}
    val bis = new BufferedInputStream(new FileInputStream(sourcePath))
    val header = lineToHeader(bis.readLine())
    val outputs = Array.tabulate(header.O)(_ => bis.readLine().toInt)
    // Read all AND gates: lhs = 2*(I+1+k), and the two rhs literals
    val rhss = Array.ofDim[(Int, Int)](header.A)
    for k <- 0 until header.A do
      val lhs  = 2 * (header.I + 1 + k)
      rhss(k)  = bis.readAndGate(lhs)
    bis.close()

    // Collect transitive fanin of the selected output (visited set by gate index)
    val needed = mutable.BitSet()
    def fanin(lit: Int): Unit =
      val v = lit >> 1
      if v == 0 || v <= header.I then return   // constant or input
      val k = v - header.I - 1
      if k < 0 || k >= header.A || needed(k) then return
      needed += k
      fanin(rhss(k)._1)
      fanin(rhss(k)._2)
    fanin(outputs(outputIndex))

    // Assign new variable indices to reachable gates (in original order)
    val reachable = (0 until header.A).filter(needed).toArray
    val newVar    = Array.fill(header.A)(-1)
    var nextNew   = header.I + 1
    reachable.foreach { k => newVar(k) = nextNew; nextNew += 1 }

    def remap(lit: Int): Int =
      val v = lit >> 1; val pol = lit & 1
      if v == 0 || v <= header.I then lit
      else (newVar(v - header.I - 1) << 1) | pol

    val newA   = reachable.length
    val newM   = header.I + newA
    val outLit = remap(outputs(outputIndex))

    val sb = new StringBuilder
    sb.append(s"aag $newM ${header.I} 0 1 $newA\n")
    for i <- 1 to header.I do sb.append(s"${2 * i}\n")
    sb.append(s"$outLit\n")
    for k <- reachable do
      val lhs = newVar(k) << 1
      sb.append(s"$lhs ${remap(rhss(k)._1)} ${remap(rhss(k)._2)}\n")
    Files.writeString(Paths.get(destPath), sb.toString)

  extension (b:BufferedInputStream) {
    def readLine() : String = {
      val c = b.read
      if c == 10 || c == -1 then "" else s"${c.toChar}${readLine()}"
    }

    def readNumber() : Int = {
      val c = b.read
      if c>>7 == 0 then c else c-128+128*readNumber()
    }

    def readAndGate(lhs:Int):(Int, Int) = {
      val d0 = readNumber()
      val d1 = readNumber()
      val rhs0 = lhs - d0
      val rhs1 = rhs0 - d1
      (rhs0, rhs1)
    }

  }
}
