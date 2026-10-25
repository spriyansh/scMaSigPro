# Create a ScMaSigPro object from counts and cell metadata

`create_scmp()` creates a
[ScMaSigPro](https://spriyansh.github.io/scMaSigPro/reference/ScMaSigPro-class.md)
object from a count matrix and a table of cell metadata with the
pseudotime and the path of each cell, such as the result of
[`assign_paths()`](https://spriyansh.github.io/scMaSigPro/reference/assign_paths.md).
Bins made elsewhere can be given too.

## Usage

``` r
create_scmp(
  counts,
  cell_data,
  feature_data = NULL,
  bin_counts = NULL,
  bin_cell_data = NULL,
  ptime_col,
  path_col,
  use_as_bin = FALSE
)
```

## Arguments

- counts:

  A matrix of raw counts, genes by cells.

- cell_data:

  A data frame of cell metadata, one row per column of `counts`, named
  like them.

- feature_data:

  An optional data frame of gene metadata, one row per gene of `counts`,
  named like them or with automatic row names. It becomes the
  `rowData()` of the `Sparse` slot, and of the `Dense` slot when this
  function builds it.

- bin_counts:

  Optional pseudo-bulk counts of bins made outside scMaSigPro: a matrix
  with the genes of `counts` as rows, in the same order, and one column
  per bin. Needs `bin_cell_data`; not with `use_as_bin = TRUE`.

- bin_cell_data:

  A data frame with one row per bin, named like the columns of
  `bin_counts`, with the binned pseudotime in `ptime_col` and the path
  in `path_col`. A `scmp_bin_size` column gives the number of cells of
  each bin; without it the sizes are `NA`.

- ptime_col:

  The name of the column of the cell metadata that holds the pseudotime
  of each cell.

- path_col:

  The name of the column of the cell or bin metadata that holds the path
  label of each cell or bin.

- use_as_bin:

  Whether every cell is a bin of its own, without pseudo-bulking, as for
  bulk time-course data.

## Value

A
[ScMaSigPro](https://spriyansh.github.io/scMaSigPro/reference/ScMaSigPro-class.md)
object with the cells in the `Sparse` slot and, with `use_as_bin = TRUE`
or `bin_counts`, the bins in the `Dense` slot.

## Details

By default the `Dense` slot is empty and
[`sc.squeeze()`](https://spriyansh.github.io/scMaSigPro/reference/sc.squeeze.md)
bins the cells. With `use_as_bin = TRUE` every cell is a bin. With
`bin_counts` and `bin_cell_data` it holds bins made outside scMaSigPro
and the workflow continues with
[`sc.set.poly()`](https://spriyansh.github.io/scMaSigPro/reference/sc.set.poly.md);
such bins have no member cells, so
[`pb_counts()`](https://spriyansh.github.io/scMaSigPro/reference/pb_counts.md)
and
[`sc.restruct()`](https://spriyansh.github.io/scMaSigPro/reference/sc.restruct.md)
do not apply.

## See also

Other input helpers:
[`as_scmp()`](https://spriyansh.github.io/scMaSigPro/reference/as_scmp.md),
[`assign_paths()`](https://spriyansh.github.io/scMaSigPro/reference/assign_paths.md),
[`paths_from_graph()`](https://spriyansh.github.io/scMaSigPro/reference/paths_from_graph.md)

## Author

Priyansh Srivastava <spriyansh29@gmail.com>

## Examples

``` r
data("splat.sim", package = "scMaSigPro")
counts <- SummarizedExperiment::assay(splat.sim, "counts")
cells <- as.data.frame(SummarizedExperiment::colData(splat.sim))
scmp <- create_scmp(counts, cells, ptime_col = "Step", path_col = "Group")
scmp
#> Class: ScMaSigProClass
#> nCells: 200
#> nFeatures: 100
#> Pseudotime Range: 1 100
#> Branching Paths: Path2, Path1
```
