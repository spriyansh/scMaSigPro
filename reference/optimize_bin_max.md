# Split the bins that are too large

`optimize_bin_max()` splits every bin that holds more than `max_allowed`
points into the number of bins that `method` and `drop` give for its
points
([`estBinSize()`](https://spriyansh.github.io/scMaSigPro/reference/estBinSize.md)),
of equal width over their range; the first new bin starts at the lower
bound of the bin and the last ends at its upper bound, so the new bins
cover it without gaps. The other bins are kept.

## Usage

``` r
optimize_bin_max(
  bin_table,
  max_allowed,
  verbose = TRUE,
  time_vector,
  lbound,
  ubound,
  bin.size,
  method,
  drop
)
```

## Arguments

- bin_table:

  A data frame of bins, one row per bin, with the columns `lbound`,
  `ubound` and `bin.size`.

- max_allowed:

  The largest number of points of a bin that is kept.

- verbose:

  Whether to print a message for each bin.

- time_vector:

  The pseudotime of the cells of the path.

- lbound, ubound:

  The names of the columns of the lower and upper bounds.

- bin.size:

  The name of the column of the number of points.

- method:

  The rule that gives the number of new bins.

- drop:

  The factor that multiplies that number (`drop_fac`).

## Value

A data frame with the columns of `bin_table`, one row per bin, where
each split bin is replaced by its parts.

## Details

[`sc.squeeze()`](https://spriyansh.github.io/scMaSigPro/reference/sc.squeeze.md)
passes as `max_allowed` the maximum of Srivastava et al. (2024), Section
2.2: the mean plus the standard deviation of the sizes of the bins of
the path, set once from the bins of the rule. "Bins larger than the
maximum size are divided", and "The process is repeated until all bins
conform to the established size criteria":
[`sc.squeeze()`](https://spriyansh.github.io/scMaSigPro/reference/sc.squeeze.md)
calls `optimize_bin_max()` again, with the same `max_allowed`, until no
bin is larger.

## See also

Other binning helpers:
[`calc_bin_size()`](https://spriyansh.github.io/scMaSigPro/reference/calc_bin_size.md),
[`estBinSize()`](https://spriyansh.github.io/scMaSigPro/reference/estBinSize.md),
[`extract_interval()`](https://spriyansh.github.io/scMaSigPro/reference/extract_interval.md),
[`pb_counts()`](https://spriyansh.github.io/scMaSigPro/reference/pb_counts.md)
