# Design matrix of the polynomial model

`predictors()` returns the design matrix that
[`sc.set.poly()`](https://spriyansh.github.io/scMaSigPro/reference/sc.set.poly.md)
stores in the `Design` slot of a
[ScMaSigPro](https://spriyansh.github.io/scMaSigPro/reference/ScMaSigPro-class.md)
object: one row per bin and one column per term of the full model.
`predictors<-` replaces it.

## Usage

``` r
predictors(object, value = "missing")

predictors(object) <- value

# S4 method for class 'ScMaSigPro'
predictors(object, value = "missing")

# S4 method for class 'ScMaSigPro'
predictors(object) <- value
```

## Arguments

- object:

  A
  [ScMaSigPro](https://spriyansh.github.io/scMaSigPro/reference/ScMaSigPro-class.md)
  object.

- value:

  A matrix with the rows of the path assignment matrix
  ([`pathAssign()`](https://spriyansh.github.io/scMaSigPro/reference/pathAssign.md)),
  one per bin and with the same names, and one column per term of the
  design.

## Value

`predictors(object)`: a numeric matrix. `predictors<-` and
`predictors(object, value)`: the updated object, returned invisibly by
the latter.

## Details

`predictors(object, value)` sets `value` as
`predictors(object) <- value` does.

## See also

Other accessors:
[`cDense()`](https://spriyansh.github.io/scMaSigPro/reference/cDense.md),
[`cSparse()`](https://spriyansh.github.io/scMaSigPro/reference/cSparse.md),
[`eDense()`](https://spriyansh.github.io/scMaSigPro/reference/eDense.md),
[`eSparse()`](https://spriyansh.github.io/scMaSigPro/reference/eSparse.md),
[`pathAssign()`](https://spriyansh.github.io/scMaSigPro/reference/pathAssign.md)

## Examples

``` r
data("scmp.ob", package = "scMaSigPro")
head(predictors(scmp.ob))
#>             Path2vsPath1 scmp_binned_pseudotime scmp_binned_pseudotimexPath2
#> Path1_bin_1            0                      1                            0
#> Path1_bin_2            0                      2                            0
#> Path1_bin_3            0                      3                            0
#> Path1_bin_4            0                      4                            0
#> Path1_bin_5            0                      5                            0
#> Path1_bin_6            0                      6                            0
```
