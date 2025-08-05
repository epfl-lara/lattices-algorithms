# lattices-algorithms
This repository contains two algorithms for the word problem and normalization problem of Ortholattices and Orthocomplemented Bisemilattices

For more information, please see the paper **Formula Normalizations in Verification** 
by Simon Guilloud (EPFL), Mario Bucev (EPFL), Dragana Milovancevic (EPFL), Viktor Kunčak (EPFL) at CAV 2023, 
the 35th International Conference on Computer Aided Verification, Paris, July 2023.


## Example Usage

```scala
//> using dep "ch.epfl.lara::orthologic::1.0,url=https://github.com/epfl-lara/lattices-algorithms/releases/download/release/orthologic.jar"
import ortholattices.Helpers.*

object Test {

  def sasaki_proj(f: Formula, g: Formula): And = and(f, or(neg(f), g))
  def sasaki_imp(f: Formula, g: Formula): Or = or(neg(f), and(f, g))

  extension (f: Formula){
    def *&(g: Formula): Formula = sasaki_proj(f, g)
    def *->(g: Formula): Formula = sasaki_imp(f, g)
  }


  def main(args: Array[String]): Unit = 
    val f = x0 *-> (x1 | x0)
    println(f)
    println(f.OLnormalize)
    println(f ~ ⊤) //checks OL equivalence 
  
}
```