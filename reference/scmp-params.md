# Arguments shared by several functions

The arguments that several functions of the package take, documented
once and inherited by the help page of each function.

## Arguments

- scmpObj:

  A
  [ScMaSigPro](https://spriyansh.github.io/scMaSigPro/reference/ScMaSigPro-class.md)
  object.

- verbose:

  Whether to print progress messages.

- BPPARAM:

  A `BiocParallelParam` object that runs the per-gene fits, such as
  `BiocParallel::MulticoreParam(2)` (Linux and macOS) or
  `BiocParallel::SnowParam(2)` (any system). The default,
  [`BiocParallel::SerialParam()`](https://rdrr.io/pkg/BiocParallel/man/SerialParam-class.html),
  fits the genes one after another; create it with `progressbar = TRUE`
  for a progress bar. The results do not depend on the backend.

- ptime_col:

  The name of the column of the cell metadata that holds the pseudotime
  of each cell.

- path_col:

  The name of the column of the cell or bin metadata that holds the path
  label of each cell or bin.

- bin_ptime_col:

  The name of the column of the bin metadata that holds the binned
  pseudotime of each bin.

- bin_col:

  The name of the column of the bin metadata that holds the name of each
  bin.

- bin_size_col:

  The name of the column of the bin metadata that holds the number of
  cells of each bin.

- bin_mem_col:

  The name of the column of the bin metadata that holds the cells of
  each bin, separated by `|`.

- assay_name:

  The name of the assay of the `Sparse` slot that holds the counts of
  the cells.

- aggregate:

  How the counts of the cells of a bin are combined: `"sum"` or `"mean"`
  (rounded to whole numbers).

- includeInflu:

  Whether to keep the genes with influential observations (see
  [`showInflu()`](https://spriyansh.github.io/scMaSigPro/reference/showSol.md)).

- feature_id:

  The name of a gene.

## See also

[scMaSigPro-package](https://spriyansh.github.io/scMaSigPro/reference/scMaSigPro-package.md)
