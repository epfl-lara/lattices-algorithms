# lattices-algorithms

Scala library implementing algorithms for the word problem and normalization problem of Ortholattices and Orthocomplemented Bisemilattices.

## Paper

**Formula Normalizations in Verification**  
Simon Guilloud (EPFL), Mario Bucev (EPFL), Dragana Milovancevic (EPFL), Viktor Kunčak (EPFL)  
CAV 2023 - 35th International Conference on Computer Aided Verification, Paris, July 2023

The complete paper artifact (C/OCaml/Scala implementations, benchmarks, scripts, experimental results) is archived separately.

## Installation

Published JAR available via GitHub releases:

```scala
//> using dep "ch.epfl.lara::orthologic::1.0,url=https://github.com/epfl-lara/lattices-algorithms/releases/download/release/orthologic.jar"
```

Or build from source:

```bash
sbt compile
sbt test
sbt assembly  # produces target/scala-3.7.1/orthologic.jar
```

## Usage Example

```scala
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

## Library Structure

**Main algorithms:**
- `algorithms/OLAlgorithm.scala` - Core orthologic normalization
- `algorithms/EntailmentAlgorithm.scala` - Entailment checking
- `algorithms/OLAlgorithmStructural.scala` - Structure-preserving variant

**Parsers and generators:**
- `AigerParser.scala` / `AigerWriter.scala` - AIGER circuit format I/O
- `FormulaGenerator.scala` - Formula generation utilities
- `CircuitGenerator.scala` - Circuit generation

**Utilities:**
- `Helpers.scala` - DSL and convenience functions
- `algorithms/Datastructures.scala` - Core data structures
- `algorithms/Printer.scala` - Pretty printing