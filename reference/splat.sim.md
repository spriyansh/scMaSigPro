# Simulated bifurcating trajectory

A `SingleCellExperiment` with the counts of 100 genes in 200 cells,
simulated with splatter (Zappia et al. 2017) along two paths that start
from the same point. The vignettes [Quick
start](https://spriyansh.github.io/scMaSigPro/articles/scMaSigPro.html)
and [Model and
tuning](https://spriyansh.github.io/scMaSigPro/articles/model-and-tuning.html)
analyse it.

## Usage

``` r
data("splat.sim", package = "scMaSigPro")
```

## Format

A `SingleCellExperiment` with 100 genes (rows) and 200 cells (columns):

- `counts` assay:

  The simulated counts.

- `colData`:

  `Cell`, the cell name; `Batch`, the batch (one, `"Batch1"`); `Group`,
  the path of the cell, a factor with the levels `"Path1"` and
  `"Path2"`; `ExpLibSize`, its expected library size; `Step`, how far
  along its path the cell is (1 to 100), used as the pseudotime;
  `sizeFactor`, the size factor of `scuttle::logNormCounts()`.

- `rowData`:

  `Gene`, the gene name; `BaseGeneMean`, the base mean of the gene;
  `OutlierFactor`, its outlier factor; `GeneMean`, its mean after the
  outlier factor; `LocalDEFacPath1` and `LocalDEFacPath2`, its
  differential expression factor along each path; `DEFacPath1` and
  `DEFacPath2`, its factor at the end of each path; `SigmaFacPath1` and
  `SigmaFacPath2`, the factor of its non-linear change along each path
  (0 for a linear change).

- `reducedDims`:

  `PCA`, the first two principal components of the log-normalised
  counts.

## Source

Simulated with `splatter::splatSimulatePaths()` (splatter 1.26.0,
`set.seed(123)`, 200 cells, 100 genes, two paths of 100 steps from the
origin, `de.prob = 0.3`, `de.facLoc = 0.2`), normalised with
`scuttle::logNormCounts()` and reduced with `scater::runPCA()`. The
script is
`system.file("scripts", "make-splat-sim.R", package = "scMaSigPro")`.

## References

Zappia L, Phipson B, Oshlack A (2017). Splatter: simulation of
single-cell RNA sequencing data. Genome Biology 18, 174.
[doi:10.1186/s13059-017-1305-0](https://doi.org/10.1186/s13059-017-1305-0)

## See also

Other datasets:
[`multi.lin.sce`](https://spriyansh.github.io/scMaSigPro/reference/multi.lin.sce.md),
[`scmp.ob`](https://spriyansh.github.io/scMaSigPro/reference/scmp.ob.md)

## Author

Priyansh Srivastava <spriyansh29@gmail.com>

## Examples

``` r
data("splat.sim", package = "scMaSigPro")
table(SummarizedExperiment::colData(splat.sim)$Group)
#> 
#> Path1 Path2 
#>    97   103 
```
