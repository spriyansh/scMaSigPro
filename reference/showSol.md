# Results of the term selection

Read the per-gene results that
[`sc.t.fit()`](https://spriyansh.github.io/scMaSigPro/reference/sc.t.fit.md)
stores in a
[ScMaSigPro](https://spriyansh.github.io/scMaSigPro/reference/ScMaSigPro-class.md)
object:

- `showSol()`: for each gene whose selected model has a significant
  coefficient, the p-value and R-squared of that model and the p-value
  of each coefficient of the full model (`NA` for the terms it leaves
  out).

- `showCoeff()`: the coefficients of these genes, 0 for the terms the
  model leaves out and `NA` for the terms that cannot be estimated.

- `showTS()`: their t statistics.

- `showGroupCoeff()`: the coefficients of the polynomial of each path.

- `showInflu()`: the genes with influential observations, one column per
  gene and one row per bin; an entry names the influence measures that
  flag the bin (DFFITS and Cook's distance, see
  [`stats::influence.measures()`](https://rdrr.io/r/stats/influence.measures.html)).

- `showSigProf()`: the pseudo-bulk counts of the genes of `showSol()`.

## Usage

``` r
showCoeff(scmpObj, includeInflu = TRUE)

showInflu(scmpObj)

showTS(scmpObj, includeInflu = TRUE)

showSol(scmpObj, includeInflu = TRUE)

showSigProf(scmpObj, includeInflu = FALSE)

showGroupCoeff(scmpObj, includeInflu = TRUE)
```

## Arguments

- scmpObj:

  A
  [ScMaSigPro](https://spriyansh.github.io/scMaSigPro/reference/ScMaSigPro-class.md)
  object.

- includeInflu:

  Whether to keep the genes with influential observations, the columns
  of `showInflu()`.

## Value

`showSol()`, `showCoeff()` and `showGroupCoeff()`: a data frame with one
row per gene. `showTS()`: a numeric matrix with one row per gene.
`showInflu()`: a character matrix with one row per bin and one column
per gene, 0 x 0 when no gene has influential observations.
`showSigProf()`: a `dgCMatrix` with one row per gene and one column per
bin.

## See also

Other results:
[`queryCoeff()`](https://spriyansh.github.io/scMaSigPro/reference/queryCoeff.md),
[`scmpResults()`](https://spriyansh.github.io/scMaSigPro/reference/scmpResults.md),
[`showPoly()`](https://spriyansh.github.io/scMaSigPro/reference/showParams.md)

## Examples

``` r
data("scmp.ob", package = "scMaSigPro")
head(showSol(scmp.ob))
#>              p-value R-squared p.valor_beta0 p.valor_Path2vsPath1
#> Gene2   1.601390e-09 0.7260152  3.214402e-32                   NA
#> Gene3   3.985890e-20 0.8731628  1.206436e-18                   NA
#> Gene4   1.230465e-66 0.9553623  3.898555e-28                   NA
#> Gene9   7.977339e-35 0.9308984  1.094009e-20          0.040284675
#> Gene10 7.184117e-269 0.9894360  1.806489e-25                   NA
#> Gene11  4.309154e-03 0.5235111  4.838164e-25          0.003959874
#>        p.valor_scmp_binned_pseudotime p.valor_scmp_binned_pseudotimexPath2
#> Gene2                              NA                         2.584140e-05
#> Gene3                    1.880938e-06                         6.631152e-07
#> Gene4                              NA                         1.143322e-10
#> Gene9                    8.650328e-07                         1.763154e-05
#> Gene10                   5.736896e-08                         3.252336e-14
#> Gene11                   2.184602e-02                         1.097789e-02
head(showGroupCoeff(scmp.ob))
#>        Path1_beta0 Path1_beta1 Path2_beta0  Path2_beta1
#> Gene2     8.681729  0.00000000    8.681729 -0.025100027
#> Gene3     5.385045 -0.12620378    5.385045 -0.011839131
#> Gene4     6.886018  0.00000000    6.886018  0.106438883
#> Gene9     6.356073  0.08220353    6.502714 -0.004367325
#> Gene10    8.162824 -0.07631690    8.162824  0.122571626
#> Gene11    8.114180  0.01301718    8.239672 -0.007985672
colnames(showInflu(scmp.ob))
#> [1] "Gene27" "Gene39" "Gene40" "Gene66" "Gene78" "Gene80" "Gene81" "Gene97"
```
