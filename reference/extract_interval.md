# Bins of equal width

`extract_interval()` divides the range of `time.vector` into `nBins`
bins of equal width, whose bounds are the breaks
`seq(min, max, length.out = nBins + 1)`, and counts the points of each
bin.

## Usage

``` r
extract_interval(time.vector, nBins = 1, bin.size, lbound, ubound)
```

## Arguments

- time.vector:

  The pseudotime values.

- nBins:

  The number of bins.

- bin.size:

  The name of the column for the number of points.

- lbound, ubound:

  The names of the columns for the lower and upper bounds.

## Value

A data frame with one row per bin, in increasing order, and the columns
`lbound`, `ubound` and `bin.size`.

## See also

Other binning helpers:
[`calc_bin_size()`](https://spriyansh.github.io/scMaSigPro/reference/calc_bin_size.md),
[`estBinSize()`](https://spriyansh.github.io/scMaSigPro/reference/estBinSize.md),
[`optimize_bin_max()`](https://spriyansh.github.io/scMaSigPro/reference/optimize_bin_max.md),
[`pb_counts()`](https://spriyansh.github.io/scMaSigPro/reference/pb_counts.md)
