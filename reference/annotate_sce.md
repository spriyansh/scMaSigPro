# Rename the pseudotime and path columns of a SingleCellExperiment

`annotate_sce()` renames the columns `exist_ptime_col` and
`exist_path_col` of the cell metadata of `sce` to `ptime_col` and
`path_col` when `labels_exist = TRUE`, and returns `sce` unchanged
otherwise.

## Usage

``` r
annotate_sce(
  sce,
  ptime_col = "Pseudotime",
  path_prefix = "Path",
  root_label = "root",
  path_col = path_prefix,
  exist_ptime_col = NULL,
  exist_path_col = NULL,
  labels_exist = FALSE,
  verbose = TRUE
)
```

## Arguments

- sce:

  A `SingleCellExperiment`.

- ptime_col:

  The name of the column of the cell metadata that holds the pseudotime
  of each cell.

- path_prefix:

  The default of `path_col`, kept in the parameters.

- root_label:

  The label of the root cells, kept in the parameters.

- path_col:

  The name of the column of the cell or bin metadata that holds the path
  label of each cell or bin.

- exist_ptime_col:

  The column with the pseudotime.

- exist_path_col:

  The column with the path labels.

- labels_exist:

  Whether to rename the columns.

- verbose:

  Whether to print progress messages.

## Value

`sce` with the renamed columns.

## See also

[`as_scmp()`](https://spriyansh.github.io/scMaSigPro/reference/as_scmp.md),
which calls it.

## Author

Priyansh Srivastava <spriyansh29@gmail.com>
