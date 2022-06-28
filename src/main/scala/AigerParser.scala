import java.io.{BufferedInputStream, FileInputStream}
import Datastructures.*
import scala.collection.mutable

object AigerParser {

  def getAigerFormulas(path:String): List[Formula] ={

    val bis = new BufferedInputStream(new FileInputStream(path))
    val header:Header = lineToHeader(bis.readLine())
    if header.L != 0 then throw new Exception("Only combinatorial circuits without latches are supported. L must be equal to 0")
    var outputs : List[Int] = Nil

    for (i <- 1 to header.O) {
      val l = bis.readLine()
      outputs = l.toInt :: outputs
    }

    val table:mutable.HashMap[Int, Formula] = mutable.HashMap()

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
    outputs map table
  }



  def lineToHeader(line:String): Header = {
    val s"aig $m $i $l $o $a" = line
    Header(m.toInt, i.toInt, l.toInt, o.toInt, a.toInt)
  }
  case class Header(M:Int, I:Int, L:Int, O:Int, A:Int)

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
