# Bin the cells of each path along pseudotime

`sc.squeeze()` divides the pseudotime range of each path into bins of
equal width and adds up the counts of the cells of each bin into
pseudo-bulk counts. The number of bins of a path is that of a histogram
rule
([`estBinSize()`](https://spriyansh.github.io/scMaSigPro/reference/estBinSize.md))
times `drop_fac`, rounded up.

Each cell goes to the bin whose interval holds its pseudotime. As in
Srivastava et al. (2024), Section 2.1, "bins are structured as
left-closed and right-open intervals, except for the last bin, which is
closed on both ends": a cell on the bound of two bins goes to the later
one. The bounds are stored in the `scmp_l_bound` and `scmp_u_bound`
columns of the bin metadata
([`cDense()`](https://spriyansh.github.io/scMaSigPro/reference/cDense.md)).

The paths are binned, and their bins stored, in the order of the levels
of the path column when it is a factor, and otherwise in the C-locale
(byte) order of the labels, whatever the collation of the session.
[`sc.set.poly()`](https://spriyansh.github.io/scMaSigPro/reference/sc.set.poly.md)
takes the first path as the reference unless told otherwise.

## Usage

``` r
sc.squeeze(
  scmpObj,
  ptime_col = scmpObj@Parameters@ptime_col,
  path_col = scmpObj@Parameters@path_col,
  bin_method = "Sturges",
  drop_fac = 1,
  verbose = FALSE,
  bin_mem_col = "scmp_bin_members",
  bin_col = "scmp_bin",
  bin_size_col = "scmp_bin_size",
  bin_ptime_col = "scmp_binned_pseudotime",
  split_bins = FALSE,
  prune_bins = FALSE,
  assay_name = "counts",
  drop_trails = FALSE,
  aggregate = "sum",
  fill_gaps = FALSE,
  additional_params = list(use_unique_time_points = FALSE)
)
```

## Arguments

- scmpObj:

  A
  [ScMaSigPro](https://spriyansh.github.io/scMaSigPro/reference/ScMaSigPro-class.md)
  object.

- ptime_col:

  The name of the column of the cell metadata that holds the pseudotime
  of each cell.

- path_col:

  The name of the column of the cell or bin metadata that holds the path
  label of each cell or bin.

- bin_method:

  The rule for the number of bins: `"Sturges"`, `"Sqrt"`, `"Rice"`,
  `"Doane"`, `"Freedman.Diaconis"` or `"Scott.Normal"` (see
  [`estBinSize()`](https://spriyansh.github.io/scMaSigPro/reference/estBinSize.md)).

- drop_fac:

  The drop factor of Srivastava et al. (2024), Section 2.1, at least
  0.3: it multiplies the number of bins of the rule, and the product is
  rounded up. Values below 1 give fewer, wider bins.

- verbose:

  Whether to print progress messages.

- bin_mem_col:

  The name of the column of the bin metadata that holds the cells of
  each bin, separated by `|`.

- bin_col:

  The name of the column of the bin metadata that holds the name of each
  bin.

- bin_size_col:

  The name of the column of the bin metadata that holds the number of
  cells of each bin.

- bin_ptime_col:

  The name of the column of the bin metadata that holds the binned
  pseudotime of each bin.

- split_bins:

  Whether to divide the bins of a path that hold more cells than the
  mean plus the standard deviation of its bin sizes (see Details).

- prune_bins:

  Whether to remove the bins of a path that hold fewer cells than the
  mean minus the standard deviation of its bin sizes (see Details).

- assay_name:

  The name of the assay of the `Sparse` slot that holds the counts of
  the cells.

- drop_trails:

  Whether to drop the last bins of the longer paths, so that every path
  has the binned pseudotime of the shortest.

- aggregate:

  How the counts of the cells of a bin are combined: `"sum"` or `"mean"`
  (rounded to whole numbers).

- fill_gaps:

  Whether to renumber the bins of a path whose binned pseudotime has
  gaps, such as after pruning.

- additional_params:

  A list: `use_unique_time_points = TRUE` bins the distinct pseudotime
  values of each path instead of its cells.

## Value

The object with the bins and their pseudo-bulk counts (`bulk.counts`) in
the `Dense` slot and the binned pseudotime of each cell in the cell
metadata. The `Design`, `Profile`, `Estimate` and `Significant` slots,
computed from earlier bins, are emptied.

## Details

`split_bins` and `prune_bins` follow Section 2.2 of Srivastava et al.
(2024): "scMaSigPro calculates the mean (\\\mu\\) and standard deviation
(\\\sigma\\) of the sizes of all bins per path to establish maximum
(\\\mu+\sigma\\) and minimum (\\\mu-\sigma\\) allowable bin sizes. Bins
larger than the maximum size are divided, and those smaller than the
minimum size can be optionally removed, ensuring uniform bin sizes. The
process is repeated until all bins conform to the established size
criteria." Both limits are set once, from the bins of the rule, empty
bins included. A bin above the maximum is divided into the number of
bins that the rule and `drop_fac` give for its cells, until no bin is
larger; `sc.squeeze()` stops when a division gives back bins it has
already made. A path with a single bin keeps it. Pruning comes after the
divisions: a negative minimum removes no bin, and the cells of a removed
bin stay in the `Sparse` slot without a bin.

## References

Srivastava P, Benegas Coll M, Götz S, Nueda MJ, Conesa A (2024).
scMaSigPro: differential expression analysis along single-cell
trajectories. Bioinformatics 40(7), btae443.
[doi:10.1093/bioinformatics/btae443](https://doi.org/10.1093/bioinformatics/btae443)

## See also

[`estBinSize()`](https://spriyansh.github.io/scMaSigPro/reference/estBinSize.md)
for the rules that set the number of bins;
[`plotBinTile()`](https://spriyansh.github.io/scMaSigPro/reference/plotBinTile.md)
to see the bins.

Other workflow steps:
[`sc.cluster.trend()`](https://spriyansh.github.io/scMaSigPro/reference/sc.cluster.trend.md),
[`sc.filter()`](https://spriyansh.github.io/scMaSigPro/reference/sc.filter.md),
[`sc.p.vector()`](https://spriyansh.github.io/scMaSigPro/reference/sc.p.vector.md),
[`sc.restruct()`](https://spriyansh.github.io/scMaSigPro/reference/sc.restruct.md),
[`sc.set.poly()`](https://spriyansh.github.io/scMaSigPro/reference/sc.set.poly.md),
[`sc.t.fit()`](https://spriyansh.github.io/scMaSigPro/reference/sc.t.fit.md)

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
scmp <- sc.squeeze(scmp)
bins <- cDense(scmp)
head(bins[, c("Path", "scmp_binned_pseudotime", "scmp_bin_size")])
#>              Path scmp_binned_pseudotime scmp_bin_size
#> Path1_bin_1 Path1                      1            14
#> Path1_bin_2 Path1                      2            15
#> Path1_bin_3 Path1                      3             7
#> Path1_bin_4 Path1                      4            17
#> Path1_bin_5 Path1                      5            13
#> Path1_bin_6 Path1                      6            11
```
