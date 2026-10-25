# MatrixDesign

The design of the polynomial model of
[`sc.set.poly()`](https://spriyansh.github.io/scMaSigPro/reference/sc.set.poly.md).

## Value

An object of class `MatrixDesign`: the design of the polynomial model,
which
[`sc.set.poly()`](https://spriyansh.github.io/scMaSigPro/reference/sc.set.poly.md)
stores in the `Design` slot of a
[ScMaSigPro](https://spriyansh.github.io/scMaSigPro/reference/ScMaSigPro-class.md)
object, with the offsets that
[`sc.p.vector()`](https://spriyansh.github.io/scMaSigPro/reference/sc.p.vector.md)
adds.
[`predictors()`](https://spriyansh.github.io/scMaSigPro/reference/predictors.md)
and
[`pathAssign()`](https://spriyansh.github.io/scMaSigPro/reference/pathAssign.md)
return its matrices.

## Details

`validObject()` checks that `assignment_matrix` has the rows of
`predictor_matrix` (one per bin), that `offset` is empty or has one
value per row and that `groups.vector` has one value per column of
`predictor_matrix`.

## Slots

- `predictor_matrix`:

  The design matrix: one row per bin, one column per term
  ([`predictors()`](https://spriyansh.github.io/scMaSigPro/reference/predictors.md)).

- `groups.vector`:

  The path of each column of `predictor_matrix`: the reference path for
  its powers of pseudotime, `<path>vs<ref>` for the terms of another
  path.

- `assignment_matrix`:

  The path assignment matrix: one row per bin, with the binned
  pseudotime, a replicate index and a 0/1 column per path
  ([`pathAssign()`](https://spriyansh.github.io/scMaSigPro/reference/pathAssign.md)).

- `offset`:

  The offset of each bin, the logarithm of its size factor (see
  [`sc.p.vector()`](https://spriyansh.github.io/scMaSigPro/reference/sc.p.vector.md)),
  or an empty vector when the fits used no offset or no fit was made
  yet.

## See also

Other classes:
[`Estimates`](https://spriyansh.github.io/scMaSigPro/reference/Estimates-class.md),
[`ParameterConfig`](https://spriyansh.github.io/scMaSigPro/reference/ParameterConfig-class.md),
[`ScMaSigPro`](https://spriyansh.github.io/scMaSigPro/reference/ScMaSigPro-class.md),
[`Significant`](https://spriyansh.github.io/scMaSigPro/reference/Significant-class.md),
[`VariableProfiles`](https://spriyansh.github.io/scMaSigPro/reference/VariableProfiles-class.md),
[`updateObject,ScMaSigPro-method`](https://spriyansh.github.io/scMaSigPro/reference/updateObject-ScMaSigPro-method.md)

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
