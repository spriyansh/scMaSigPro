# Parameters and model formula of an analysis

`showParams()` lists the parameters that the steps of the workflow store
in a
[ScMaSigPro](https://spriyansh.github.io/scMaSigPro/reference/ScMaSigPro-class.md)
object (see
[ParameterConfig](https://spriyansh.github.io/scMaSigPro/reference/ParameterConfig-class.md)).
`showPoly()` writes the full model that
[`sc.set.poly()`](https://spriyansh.github.io/scMaSigPro/reference/sc.set.poly.md)
set up, with one coefficient `beta1`, `beta2`, ... per column of the
design matrix.

## Usage

``` r
showPoly(scmpObj)

showParams(scmpObj)
```

## Arguments

- scmpObj:

  A
  [ScMaSigPro](https://spriyansh.github.io/scMaSigPro/reference/ScMaSigPro-class.md)
  object.

## Value

`showParams()`: a data frame with the columns `parameters` and `value`,
one row per parameter. `showPoly()`: a character string.

## See also

Other results:
[`queryCoeff()`](https://spriyansh.github.io/scMaSigPro/reference/queryCoeff.md),
[`scmpResults()`](https://spriyansh.github.io/scMaSigPro/reference/scmpResults.md),
[`showCoeff()`](https://spriyansh.github.io/scMaSigPro/reference/showSol.md)

## Examples

``` r
data("scmp.ob", package = "scMaSigPro")
head(showParams(scmp.ob))
#>         parameters                  value
#> 1    bin_ptime_col scmp_binned_pseudotime
#> 2      path_prefix                   Path
#> 3       root_label                   root
#> 4        ptime_col             Pseudotime
#> 5 selection_method               backward
#> 6       bin_method                Sturges
showPoly(scmp.ob)
#> [1] "beta0 + beta1*Path2vsPath1 + beta2*scmp_binned_pseudotime + beta3*scmp_binned_pseudotimexPath2"
```
