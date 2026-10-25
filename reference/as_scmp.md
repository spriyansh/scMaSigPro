# Create a ScMaSigPro object from a SingleCellExperiment

`as_scmp()` creates a
[ScMaSigPro](https://spriyansh.github.io/scMaSigPro/reference/ScMaSigPro-class.md)
object from a `SingleCellExperiment` whose cell metadata (`colData`)
holds a pseudotime column and a path column. To start from a count
matrix and a cell table, use
[`create_scmp()`](https://spriyansh.github.io/scMaSigPro/reference/create_scmp.md);
[`assign_paths()`](https://spriyansh.github.io/scMaSigPro/reference/assign_paths.md)
and
[`paths_from_graph()`](https://spriyansh.github.io/scMaSigPro/reference/paths_from_graph.md)
build the two columns from the output of a trajectory tool.

## Usage

``` r
as_scmp(
  object,
  from = "sce",
  path_prefix = "Path",
  root_label = "root",
  ptime_col = "Pseudotime",
  path_col = path_prefix,
  verbose = TRUE,
  additional_params = list(labels_exist = FALSE)
)
```

## Arguments

- object:

  A `SingleCellExperiment` with the counts of the cells.

- from:

  The class of `object`; only `"sce"` is supported.

- path_prefix:

  The default of `path_col`, kept in the parameters.

- root_label:

  The label of the root cells, kept in the parameters.

- ptime_col:

  The name of the column of the cell metadata that holds the pseudotime
  of each cell.

- path_col:

  The name of the column of the cell or bin metadata that holds the path
  label of each cell or bin.

- verbose:

  Whether to print progress messages.

- additional_params:

  A list: with `labels_exist = TRUE`, the columns `exist_ptime_col` and
  `exist_path_col` of the cell metadata are renamed to `ptime_col` and
  `path_col`.

## Value

A
[ScMaSigPro](https://spriyansh.github.io/scMaSigPro/reference/ScMaSigPro-class.md)
object with the cells in the `Sparse` slot.

## See also

Other input helpers:
[`assign_paths()`](https://spriyansh.github.io/scMaSigPro/reference/assign_paths.md),
[`create_scmp()`](https://spriyansh.github.io/scMaSigPro/reference/create_scmp.md),
[`paths_from_graph()`](https://spriyansh.github.io/scMaSigPro/reference/paths_from_graph.md)

## Author

Priyansh Srivastava <spriyansh29@gmail.com>

## Examples

``` r
data("splat.sim", package = "scMaSigPro")
scmp <- as_scmp(splat.sim,
    additional_params = list(
        labels_exist = TRUE, exist_ptime_col = "Step",
        exist_path_col = "Group"
    ),
    verbose = FALSE
)
scmp
#> Class: ScMaSigProClass
#> nCells: 200
#> nFeatures: 100
#> Pseudotime Range: 1 100
#> Branching Paths: Path2, Path1
```
