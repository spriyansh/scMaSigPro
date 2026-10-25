# Pseudo-bulk counts of the bins

`pb_counts()` adds up, or averages, the counts of the cells of each bin
into the pseudo-bulk counts (`bulk.counts`) of the `Dense` slot.
[`sc.squeeze()`](https://spriyansh.github.io/scMaSigPro/reference/sc.squeeze.md)
and
[`sc.restruct()`](https://spriyansh.github.io/scMaSigPro/reference/sc.restruct.md)
call it; call it after changing the bins with
[`cDense()`](https://spriyansh.github.io/scMaSigPro/reference/cDense.md).

## Usage

``` r
pb_counts(
  scmpObj,
  bin_mem_col = scmpObj@Parameters@bin_mem_col,
  bin_col = scmpObj@Parameters@bin_col,
  assay_name = "counts",
  cluster_count_by = "sum"
)
```

## Arguments

- scmpObj:

  A
  [ScMaSigPro](https://spriyansh.github.io/scMaSigPro/reference/ScMaSigPro-class.md)
  object.

- bin_mem_col:

  The name of the column of the bin metadata that holds the cells of
  each bin, separated by `|`.

- bin_col:

  The name of the column of the bin metadata that holds the name of each
  bin.

- assay_name:

  The name of the assay of the `Sparse` slot that holds the counts of
  the cells.

- cluster_count_by:

  How the counts of the cells of a bin are combined: `"sum"` or `"mean"`
  (rounded to whole numbers).

## Value

The object whose `Dense` slot holds the pseudo-bulk counts
(`bulk.counts`, genes by bins) and the bin metadata.

## Details

The bins are the rows of the bin metadata
([`cDense()`](https://spriyansh.github.io/scMaSigPro/reference/cDense.md)):
their row names, or the `bin_col` column when there are none, name the
columns of the counts, and `bin_mem_col` gives their cells.

## References

Srivastava P, Benegas Coll M, Götz S, Nueda MJ, Conesa A (2024).
scMaSigPro: differential expression analysis along single-cell
trajectories. Bioinformatics 40(7), btae443.
[doi:10.1093/bioinformatics/btae443](https://doi.org/10.1093/bioinformatics/btae443)

## See also

Other binning helpers:
[`calc_bin_size()`](https://spriyansh.github.io/scMaSigPro/reference/calc_bin_size.md),
[`estBinSize()`](https://spriyansh.github.io/scMaSigPro/reference/estBinSize.md),
[`extract_interval()`](https://spriyansh.github.io/scMaSigPro/reference/extract_interval.md),
[`optimize_bin_max()`](https://spriyansh.github.io/scMaSigPro/reference/optimize_bin_max.md)

## Author

Priyansh Srivastava <spriyansh29@gmail.com>

## Examples

``` r
data("scmp.ob", package = "scMaSigPro")
# The mean instead of the sum of the counts of the cells of each bin
scmp <- pb_counts(scmp.ob, cluster_count_by = "mean")
eDense(scmp)[1:3, 1:4]
#>       Path1_bin_1 Path1_bin_2 Path1_bin_3 Path1_bin_4
#> Gene1           1           2           2           2
#> Gene2         452         489         482         539
#> Gene3          14          16          15          13
```
