# Path assignment matrix of the design

`pathAssign()` returns the path assignment matrix that
[`sc.set.poly()`](https://spriyansh.github.io/scMaSigPro/reference/sc.set.poly.md)
stores in the `Design` slot of a
[ScMaSigPro](https://spriyansh.github.io/scMaSigPro/reference/ScMaSigPro-class.md)
object: one row per bin, with the binned pseudotime, a replicate index
and a 0/1 column per path. `pathAssign<-` replaces it.

## Usage

``` r
pathAssign(object, value = "missing")

pathAssign(object) <- value

# S4 method for class 'ScMaSigPro'
pathAssign(object, value = "missing")

# S4 method for class 'ScMaSigPro'
pathAssign(object) <- value
```

## Arguments

- object:

  A
  [ScMaSigPro](https://spriyansh.github.io/scMaSigPro/reference/ScMaSigPro-class.md)
  object.

- value:

  A matrix with the rows of the design matrix
  ([`predictors()`](https://spriyansh.github.io/scMaSigPro/reference/predictors.md)),
  one per bin and with the same names.

## Value

`pathAssign(object)`: a numeric matrix. `pathAssign<-` and
`pathAssign(object, value)`: the updated object, returned invisibly by
the latter.

## Details

`pathAssign(object, value)` sets `value` as
`pathAssign(object) <- value` does.

## See also

Other accessors:
[`cDense()`](https://spriyansh.github.io/scMaSigPro/reference/cDense.md),
[`cSparse()`](https://spriyansh.github.io/scMaSigPro/reference/cSparse.md),
[`eDense()`](https://spriyansh.github.io/scMaSigPro/reference/eDense.md),
[`eSparse()`](https://spriyansh.github.io/scMaSigPro/reference/eSparse.md),
[`predictors()`](https://spriyansh.github.io/scMaSigPro/reference/predictors.md)

## Examples

``` r
data("scmp.ob", package = "scMaSigPro")
head(pathAssign(scmp.ob))
#>             scmp_binned_pseudotime Replicate Path1 Path2
#> Path1_bin_1                      1         1     1     0
#> Path1_bin_2                      2         2     1     0
#> Path1_bin_3                      3         3     1     0
#> Path1_bin_4                      4         4     1     0
#> Path1_bin_5                      5         5     1     0
#> Path1_bin_6                      6         6     1     0
```
