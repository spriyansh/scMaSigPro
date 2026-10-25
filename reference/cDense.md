# Bin metadata of the Dense slot

`cDense()` returns the bin metadata (`colData`) of the `Dense` slot of a
[ScMaSigPro](https://spriyansh.github.io/scMaSigPro/reference/ScMaSigPro-class.md)
object, one row per pseudotime bin; `cDense<-` replaces it.

## Usage

``` r
cDense(object, value = "missing")

cDense(object) <- value

# S4 method for class 'ScMaSigPro'
cDense(object, value = "missing")

# S4 method for class 'ScMaSigPro'
cDense(object) <- value
```

## Arguments

- object:

  A
  [ScMaSigPro](https://spriyansh.github.io/scMaSigPro/reference/ScMaSigPro-class.md)
  object.

- value:

  A data frame of bin metadata, one row per bin.

## Value

`cDense(object)`: a data frame with one row per bin. `cDense<-` and
`cDense(object, value)`: the updated object, returned invisibly by the
latter.

## Details

The bins are the columns of the pseudo-bulk counts. A `value` with one
row per bin replaces the metadata and keeps the counts; its rows may be
renamed, and
[`pb_counts()`](https://spriyansh.github.io/scMaSigPro/reference/pb_counts.md)
then recomputes the counts. A `value` with another number of rows, such
as bins rebuilt by hand, cannot keep the counts: the `Dense` slot then
holds the metadata without counts, a message says so, and
[`pb_counts()`](https://spriyansh.github.io/scMaSigPro/reference/pb_counts.md)
must be run before the next step. `cDense(object, value)` sets `value`
as `cDense(object) <- value` does.

## See also

Other accessors:
[`cSparse()`](https://spriyansh.github.io/scMaSigPro/reference/cSparse.md),
[`eDense()`](https://spriyansh.github.io/scMaSigPro/reference/eDense.md),
[`eSparse()`](https://spriyansh.github.io/scMaSigPro/reference/eSparse.md),
[`pathAssign()`](https://spriyansh.github.io/scMaSigPro/reference/pathAssign.md),
[`predictors()`](https://spriyansh.github.io/scMaSigPro/reference/predictors.md)

## Examples

``` r
data("scmp.ob", package = "scMaSigPro")
bins <- cDense(scmp.ob)
head(bins[, c("Path", "scmp_binned_pseudotime", "scmp_bin_size")])
#>              Path scmp_binned_pseudotime scmp_bin_size
#> Path1_bin_1 Path1                      1            14
#> Path1_bin_2 Path1                      2            15
#> Path1_bin_3 Path1                      3             7
#> Path1_bin_4 Path1                      4            17
#> Path1_bin_5 Path1                      5            13
#> Path1_bin_6 Path1                      6            11

# Add a column to the bin metadata
bins$early <- bins$scmp_binned_pseudotime <= 4
cDense(scmp.ob) <- bins
```
