# Number of cells of a bin

`calc_bin_size()` counts the cells of a bin: the names separated by `|`
in its `clus_mem_col` column.

## Usage

``` r
calc_bin_size(x, clus_mem_col = "scmp_cluster_members")
```

## Arguments

- x:

  A row of the bin metadata.

- clus_mem_col:

  The name of the column with the cells of the bin.

## Value

The number of cells of the bin.

## See also

Other binning helpers:
[`estBinSize()`](https://spriyansh.github.io/scMaSigPro/reference/estBinSize.md),
[`extract_interval()`](https://spriyansh.github.io/scMaSigPro/reference/extract_interval.md),
[`optimize_bin_max()`](https://spriyansh.github.io/scMaSigPro/reference/optimize_bin_max.md),
[`pb_counts()`](https://spriyansh.github.io/scMaSigPro/reference/pb_counts.md)

## Author

Priyansh Srivastava <spriyansh29@gmail.com>
