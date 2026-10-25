# ScMaSigPro

An analysis of the scMaSigPro workflow: the data at the level of the
cells and of the pseudotime bins, as two `SingleCellExperiment` objects,
and the results and settings of every step.

## Value

An object of class `ScMaSigPro`.
[`as_scmp()`](https://spriyansh.github.io/scMaSigPro/reference/as_scmp.md)
and
[`create_scmp()`](https://spriyansh.github.io/scMaSigPro/reference/create_scmp.md)
make one with the cells in the `Sparse` slot;
[`sc.squeeze()`](https://spriyansh.github.io/scMaSigPro/reference/sc.squeeze.md)
adds the bins (`Dense`),
[`sc.set.poly()`](https://spriyansh.github.io/scMaSigPro/reference/sc.set.poly.md)
the design (`Design`),
[`sc.p.vector()`](https://spriyansh.github.io/scMaSigPro/reference/sc.p.vector.md)
the test of each gene (`Profile`),
[`sc.t.fit()`](https://spriyansh.github.io/scMaSigPro/reference/sc.t.fit.md)
the selected models (`Estimate`),
[`sc.filter()`](https://spriyansh.github.io/scMaSigPro/reference/sc.filter.md)
and
[`sc.cluster.trend()`](https://spriyansh.github.io/scMaSigPro/reference/sc.cluster.trend.md)
the gene lists and clusters (`Significant`), and every step its settings
(`Parameters`).

## Details

`validObject()` checks every slot with the validity method of its class
and reports each problem with the name of the slot; `version` must be a
single version number. The constructors, the steps of the workflow,
[`pb_counts()`](https://spriyansh.github.io/scMaSigPro/reference/pb_counts.md)
and the setters check the object they return, so a step or setter that
would return an invalid object stops with these problems instead.
Objects saved by an earlier version of the package, for example with
[`saveRDS()`](https://rdrr.io/r/base/readRDS.html), are brought up to
date with
[`updateObject()`](https://spriyansh.github.io/scMaSigPro/reference/updateObject-ScMaSigPro-method.md).

## Slots

- `Sparse`:

  The cells: a `SingleCellExperiment` with their counts and metadata,
  including the pseudotime and path columns.

- `Dense`:

  The pseudotime bins: a `SingleCellExperiment` with their pseudo-bulk
  counts and metadata.

- `Design`:

  The design, a
  [MatrixDesign](https://spriyansh.github.io/scMaSigPro/reference/MatrixDesign-class.md)
  object.

- `Profile`:

  The test of each gene, a
  [VariableProfiles](https://spriyansh.github.io/scMaSigPro/reference/VariableProfiles-class.md)
  object.

- `Estimate`:

  The selected models, an
  [Estimates](https://spriyansh.github.io/scMaSigPro/reference/Estimates-class.md)
  object.

- `Significant`:

  The gene lists and clusters, a
  [Significant](https://spriyansh.github.io/scMaSigPro/reference/Significant-class.md)
  object.

- `Parameters`:

  The settings, a
  [ParameterConfig](https://spriyansh.github.io/scMaSigPro/reference/ParameterConfig-class.md)
  object.

- `version`:

  The version of scMaSigPro that created the object or last changed its
  structure in
  [`updateObject()`](https://spriyansh.github.io/scMaSigPro/reference/updateObject-ScMaSigPro-method.md).

## Why a class of its own

`ScMaSigPro` holds two `SingleCellExperiment` objects because the
analysis works at two resolutions whose columns differ. `Sparse` holds
the cells, with their pseudotime and path labels. `Dense` holds the
pseudotime bins (the pseudo-bulk) to which the polynomial GLMs are
fitted. One `SingleCellExperiment` cannot hold both: `altExp()` needs
the columns of the main experiment, and bins stored in `metadata()`
would lose the validity checks and accessors of an experiment. The other
slots hold results that have no place in the SummarizedExperiment model:
the design matrices (`MatrixDesign`), the per-gene fits of the full
model (`VariableProfiles`), the coefficients and significance of the
selected models (`Estimates`), the gene lists and clusters
(`Significant`) and a record of every parameter used
(`ParameterConfig`), for reproducibility. Both experiments are built and
changed through the SummarizedExperiment accessors only, and
`validObject()` runs after every step of the workflow. To use the
results with other tools,
[`scmpResults()`](https://spriyansh.github.io/scMaSigPro/reference/scmpResults.md)
returns them as a `DataFrame` with one row per gene, and
[`scmpToRowData()`](https://spriyansh.github.io/scMaSigPro/reference/scmpResults.md)
writes them into the `rowData()` of a `SingleCellExperiment`.

## See also

Other classes:
[`Estimates`](https://spriyansh.github.io/scMaSigPro/reference/Estimates-class.md),
[`MatrixDesign`](https://spriyansh.github.io/scMaSigPro/reference/MatrixDesign-class.md),
[`ParameterConfig`](https://spriyansh.github.io/scMaSigPro/reference/ParameterConfig-class.md),
[`Significant`](https://spriyansh.github.io/scMaSigPro/reference/Significant-class.md),
[`VariableProfiles`](https://spriyansh.github.io/scMaSigPro/reference/VariableProfiles-class.md),
[`updateObject,ScMaSigPro-method`](https://spriyansh.github.io/scMaSigPro/reference/updateObject-ScMaSigPro-method.md)

## Examples

``` r
data("scmp.ob", package = "scMaSigPro")
scmp.ob
#> Class: ScMaSigProClass
#> nCells: 200
#> nFeatures: 100
#> Pseudotime Range: 1 100
#> Branching Paths: Path2, Path1
#> Binned Pseudotime: 1-8(Range), 4.5(Mean), 
#> Number of bins-> Path1: 8 Path2: 8
#> Average bin Size-> Path1: 12 Path2: 13
#> Polynomial Order: 1
#> No. of Significant Profiles: 50
#> No. of Influential Features: 8
validObject(scmp.ob)
#> [1] TRUE
```
