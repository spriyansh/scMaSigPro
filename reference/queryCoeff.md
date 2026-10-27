# Query the coefficients of the selected models

`queryCoeff()` returns the coefficients of a group of terms for the
genes whose selected model
([`sc.t.fit()`](https://spriyansh.github.io/scMaSigPro/reference/sc.t.fit.md))
passes `rsq` and `p_value` and has a non-zero coefficient for every term
of the group. It also prints the formula of the model.

## Usage

``` r
queryCoeff(
  scmpObj,
  rsq = 0.7,
  p_value = scmpObj@Parameters@p_value,
  query = "pseudotime_path",
  change = NULL,
  strictly = FALSE,
  verbose = TRUE
)
```

## Arguments

- scmpObj:

  A
  [ScMaSigPro](https://spriyansh.github.io/scMaSigPro/reference/ScMaSigPro-class.md)
  object.

- rsq:

  The smallest R-squared of the selected model of a gene.

- p_value:

  The largest p-value of the selected model of a gene; by default that
  of
  [`sc.p.vector()`](https://spriyansh.github.io/scMaSigPro/reference/sc.p.vector.md).

- query:

  The terms: `"pseudotime"`, the powers of pseudotime of the reference
  path; `"pseudotime_path"` or `"path_pseudotime"`, their products with
  the path dummies; `"path"`, the path dummies.

- change:

  `NULL`, or `"increasing"` or `"decreasing"`: keep the genes whose
  coefficients of the query add up to a positive (negative) number, or
  with `strictly = TRUE` are all positive (negative).

- strictly:

  See `change`.

- verbose:

  Whether to print progress messages.

## Value

A data frame of coefficients: one row per gene that passes the
thresholds and the query, one column per queried term.

## See also

[`showCoeff()`](https://spriyansh.github.io/scMaSigPro/reference/showSol.md)
for the coefficients of every gene.

Other results:
[`scmpResults()`](https://spriyansh.github.io/scMaSigPro/reference/scmpResults.md),
[`showCoeff()`](https://spriyansh.github.io/scMaSigPro/reference/showSol.md),
[`showPoly()`](https://spriyansh.github.io/scMaSigPro/reference/showParams.md)

## Author

Priyansh Srivastava <spriyansh29@gmail.com>.

## Examples

``` r
data("scmp.ob", package = "scMaSigPro")
# Genes whose coefficients of pseudotime are all positive
queryCoeff(scmp.ob,
    query = "pseudotime", change = "increasing", strictly = TRUE,
    verbose = FALSE
)
#> Polynomial-GLM Formula: beta0 + beta1*Path2vsPath1 + beta2*scmp_binned_pseudotime + beta3*scmp_binned_pseudotimexPath2
#>        betascmp_binned_pseudotime
#> Gene9                  0.08220353
#> Gene18                 0.08788562
#> Gene25                 0.04003636
#> Gene40                 0.03289285
#> Gene46                 0.12313411
#> Gene52                 0.04574704
#> Gene62                 0.02188666
#> Gene81                 0.02422643
#> Gene83                 0.01054990
#> Gene86                 0.12192126
#> Gene89                 0.07235517
```
