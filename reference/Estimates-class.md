# Estimates

The models that
[`sc.t.fit()`](https://spriyansh.github.io/scMaSigPro/reference/sc.t.fit.md)
selected.

## Value

An object of class `Estimates`: the selected model of each gene, which
[`sc.t.fit()`](https://spriyansh.github.io/scMaSigPro/reference/sc.t.fit.md)
stores in the `Estimate` slot of a
[ScMaSigPro](https://spriyansh.github.io/scMaSigPro/reference/ScMaSigPro-class.md)
object.
[`showSol()`](https://spriyansh.github.io/scMaSigPro/reference/showSol.md)
and the other result readers return its matrices.

## Details

`validObject()` checks that `t_score_matrix` has the shape and rows of
`coefficient_matrix`, that `path_coefficient_matrix` has its rows, and
that every row of `significance_matrix` is a row of
`coefficient_matrix`.

## Slots

- `significance_matrix`:

  For each gene with a significant selected model, its p-value and
  R-squared and the p-value of each term
  ([`showSol()`](https://spriyansh.github.io/scMaSigPro/reference/showSol.md)).

- `coefficient_matrix`:

  The coefficients
  ([`showCoeff()`](https://spriyansh.github.io/scMaSigPro/reference/showSol.md)).

- `path_coefficient_matrix`:

  The coefficients of the polynomial of each path
  ([`showGroupCoeff()`](https://spriyansh.github.io/scMaSigPro/reference/showSol.md)).

- `t_score_matrix`:

  The t statistics
  ([`showTS()`](https://spriyansh.github.io/scMaSigPro/reference/showSol.md)).

- `path`:

  The path of the intercept and of each term.

- `influential`:

  The genes with influential observations
  ([`showInflu()`](https://spriyansh.github.io/scMaSigPro/reference/showSol.md)).

## See also

Other classes:
[`MatrixDesign`](https://spriyansh.github.io/scMaSigPro/reference/MatrixDesign-class.md),
[`ParameterConfig`](https://spriyansh.github.io/scMaSigPro/reference/ParameterConfig-class.md),
[`ScMaSigPro`](https://spriyansh.github.io/scMaSigPro/reference/ScMaSigPro-class.md),
[`Significant`](https://spriyansh.github.io/scMaSigPro/reference/Significant-class.md),
[`VariableProfiles`](https://spriyansh.github.io/scMaSigPro/reference/VariableProfiles-class.md),
[`updateObject,ScMaSigPro-method`](https://spriyansh.github.io/scMaSigPro/reference/updateObject-ScMaSigPro-method.md)

## Examples

``` r
data("scmp.ob", package = "scMaSigPro")
head(showCoeff(scmp.ob))
#>           beta0 betaPath2vsPath1 betascmp_binned_pseudotime
#> Gene2  8.681729        0.0000000                 0.00000000
#> Gene3  5.385045        0.0000000                -0.12620378
#> Gene4  6.886018        0.0000000                 0.00000000
#> Gene9  6.356073        0.1466414                 0.08220353
#> Gene10 8.162824        0.0000000                -0.07631690
#> Gene11 8.114180        0.1254918                 0.01301718
#>        betascmp_binned_pseudotimexPath2
#> Gene2                       -0.02510003
#> Gene3                        0.11436465
#> Gene4                        0.10643888
#> Gene9                       -0.08657086
#> Gene10                       0.19888853
#> Gene11                      -0.02100285
```
