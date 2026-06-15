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

---

## Benchmark Scripts and Graph Generation

All scripts below expect the following binaries to be compiled first:

- **C olsc**: `cd ol-vladislas/col && gcc -O2 -o olsc main.c olsc.c aiger.c map.c set.c vector.c utils.c -lm`
- **kissat**: `cd solvers/kissat && ./configure && make`
- **Scala jar**: `sbt assembly` (produces `target/scala-3.7.1/orthologic.jar`)
- **OCaml binary**: `cd ol-vladislas/ol-in-ocaml && dune build`

Benchmarks operate on an **EPFL-style circuit folder** (e.g. `benchmark3/`).
Each circuit family (adder, bar, div, …) is a subdirectory containing one sub-directory per bit position, each with:
- `phi.aag` — original AIG circuit
- `phi_norm.aag` — normalised AIG circuit
- `miter.cnf` — Tseitin CNF encoding the Boolean miter (used by SAT solvers)

---

### `eval_C_vs_kissat_benchmark.sh` — olsc vs kissat head-to-head

Runs **C olsc** (OL entailment check: `phi ⊢ phi_norm`) and **kissat** (UNSAT check on `miter.cnf`, which encodes `¬(phi_norm ⟹ phi)` classically) on every formula in a benchmark folder, printing one result line per formula immediately as it is solved.

Per-circuit skip logic: once a tool times out or errors on bit N of a circuit, all subsequent bits of that circuit are skipped for that tool (the other tool continues).

Results are sorted by formula size within each circuit family.

```
./eval_C_vs_kissat_benchmark.sh <folder> [<timeout_sec>] [<keyword>]
```

| Argument | Default | Description |
|---|---|---|
| `folder` | required | Path to the benchmark folder |
| `timeout_sec` | 30 | Per-formula timeout in seconds |
| `keyword` | (none) | If given, only process entries whose path contains this string |

**Output columns**: `formula | phi_A nrm_A inputs | cnf_V cnf_C | olsc olsc_ms | kissat ksat_ms kself_ms`

**Example**:
```bash
./eval_C_vs_kissat_benchmark.sh benchmark3 300 > benchmark3_kissat_vs_ol.txt
./eval_C_vs_kissat_benchmark.sh benchmark3 300 multiplier   # one circuit only
```

---

### `collect_stats.sh` — collect raw per-formula statistics

Runs olsc (with full internal statistics output) and kissat (with `--statistics`) on every formula and saves all outputs to a parallel data directory for later analysis. Runs are **incremental**: entries already present in the output directory are skipped.

```
./collect_stats.sh <benchmark_folder> <data_output_dir> [<timeout_sec>] [<keyword>] [<max_cnf_bytes>]
```

| Argument | Default | Description |
|---|---|---|
| `benchmark_folder` | required | Path to the benchmark folder |
| `data_output_dir` | required | Directory where raw outputs are saved |
| `timeout_sec` | 30 | Per-formula timeout |
| `keyword` | (none) | Filter: only process entries whose path contains this string |
| `max_cnf_bytes` | 0 (no limit) | Skip entries whose `miter.cnf` exceeds this size |

**Output structure**: for each formula `F`, the directory `data_output_dir/F/` contains `olsc.txt`, `kissat.txt`, `olsc_ms.txt`, `kissat_ms.txt`, and copies of the three input files.

**Example**:
```bash
./collect_stats.sh benchmark3 benchmark3_stats 300
```

---

### `benchmark_comparison.sh` — OCaml vs Scala normalisation

Compares the OCaml and Scala implementations of OL normalisation across all EPFL circuit families. Reports for each circuit: original formula size, normal form size, and wall-clock time. Results are cached in `benchmark_cache.tsv`.

```
./benchmark_comparison.sh [--only-ocaml | --only-scala]
```

| Flag | Description |
|---|---|
| (none) | Run both OCaml and Scala |
| `--only-ocaml` | Re-run OCaml; load Scala from cache |
| `--only-scala` | Re-run Scala; load OCaml from cache |

---

### `benchmark_entailment.sh` — C / OCaml / Scala entailment comparison

Runs **C olsc**, **OCaml olsc**, and **Scala** on the EPFL circuits and reports for each how many outputs were proven valid and the total time. Also has Scala cross-verify the indices claimed valid by C. Results are cached in `benchmark_entail_cache.tsv`.

```
./benchmark_entailment.sh [--only-ocaml | --only-scala | --only-c | --skip-c]
```

---

### `benchmark_computeall.sh` — C vs Scala consistency (drain-all mode)

Runs C and Scala in *compute-all* mode (drain the full worklist without stopping at the goal), then compares the number of proven sequents and formula-space size. Divergences indicate soundness or completeness bugs.

```
./benchmark_computeall.sh
```

Environment variables `TC` (default 120s) and `TS` (default 300s) control the per-call timeouts for C and Scala respectively.

---

### `benchmark_modes.sh` — C vs Scala per-mode speedup

Runs C and Scala across three entailment modes (`validity`, `reflexivity`, `equiv-renamed`) for a fixed suite of circuits. Reports the speedup ratio C/Scala and has Scala verify the indices claimed valid by C.

```
./benchmark_modes.sh
```

Environment variables `TC`, `TS`, `TV` control timeouts for C, Scala, and Scala verification respectively.

---

### `run_custom_suite.sh` — run C olsc against a custom test suite

Runs C olsc on a directory of named test cases, each containing `phi.aag`, `phi_norm.aag`, optional `axiom_*_lhs/rhs.aig` files, and `expected.txt`. Reports pass/fail for each test.

```
./run_custom_suite.sh [<olsc_binary>] [<suite_directory>]
```

| Argument | Default | Description |
|---|---|---|
| `olsc_binary` | `ol-vladislas/col/olsc` | Path to the C olsc binary |
| `suite_directory` | `CustomBenchmark` | Path to the test suite directory |

---

### `graphs/plot_benchmark.py` — generate per-circuit performance graphs

Reads a `benchmark3_kissat_vs_ol.txt` result file (as produced by `eval_C_vs_kissat_benchmark.sh`) and generates one detailed PDF+PNG graph per circuit family, comparing olsc and kissat wall-clock times as a function of circuit size.

**Graph format**: dual-line plot with log-scale Y axis (time in ms), X axis = AND-gate count in φ (or bit index for circuits with near-constant size such as `max`). TIMEOUT events are shown as `×` markers at the timeout line. Adder and bar are excluded as they are uniformly fast and not informative.

```
cd graphs
python3 plot_benchmark.py
```

Output files are written to the `graphs/` directory as `<circuit>_detail.pdf` and `<circuit>_detail.png`. The benchmark file path and timeout value are configured at the top of the script (`BENCHMARK_FILE`, `TIMEOUT_MS`).