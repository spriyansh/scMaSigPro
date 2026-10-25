# Per-gene results of an analysis

`scmpResults()` collects the per-gene results stored in a
[ScMaSigPro](https://spriyansh.github.io/scMaSigPro/reference/ScMaSigPro-class.md)
object in one table with a row per gene. `scmpToRowData()` writes that
table into the `rowData()` of a `SingleCellExperiment`, such as the one
the analysis started from, so that the results travel with the data.

## Usage

``` r
scmpResults(scmpObj)

scmpToRowData(sce, scmpObj)
```

## Arguments

- scmpObj:

  A
  [ScMaSigPro](https://spriyansh.github.io/scMaSigPro/reference/ScMaSigPro-class.md)
  object.

- sce:

  A `SingleCellExperiment`, or another `SummarizedExperiment`, whose row
  names are gene names.

## Value

`scmpResults()`: a
[`DataFrame`](https://rdrr.io/pkg/S4Vectors/man/DataFrame-class.html).
`scmpToRowData()`: `sce` with the new `rowData()` columns.

## Details

`scmpResults()` has one row per gene of the object (the rows of the
`Sparse` slot), named by gene, and the columns of the steps that have
been run:

- [`sc.p.vector()`](https://spriyansh.github.io/scMaSigPro/reference/sc.p.vector.md):
  `p_value` and `adj_p_value`, the p-value of the full model and its
  adjusted value, `NA` for the genes that were not fitted (too few
  values or only zero counts); `non_flat`, `TRUE` for the genes it
  selected.

- [`sc.t.fit()`](https://spriyansh.github.io/scMaSigPro/reference/sc.t.fit.md):
  `model_p_value` and `r_squared`, the p-value and the R-squared of the
  model selected by the stepwise regression (the `p-value` and
  `R-squared` columns of
  [`showSol()`](https://spriyansh.github.io/scMaSigPro/reference/showSol.md)),
  `NA` for the genes without a row there; `influential`, `TRUE` for the
  genes with influential observations (the columns of
  [`showInflu()`](https://spriyansh.github.io/scMaSigPro/reference/showSol.md)).

- [`sc.filter()`](https://spriyansh.github.io/scMaSigPro/reference/sc.filter.md):
  one column per gene list, named `sig_` followed by the name of the
  list, `TRUE` for the genes of the list.

- [`sc.cluster.trend()`](https://spriyansh.github.io/scMaSigPro/reference/sc.cluster.trend.md):
  `cluster`, the cluster of each clustered gene, `NA` for the other
  genes.

The logical columns are `FALSE`, never `NA`, for the genes outside a
set.

`scmpToRowData()` matches the genes by name. It removes the columns of
`rowData(sce)` whose names start with `scmp_`, as an earlier call writes
them, and adds the columns of `scmpResults()` with that prefix, such as
`scmp_p_value`. The genes of `sce` that are not in the object get `NA`
in every new column, and the genes of the object that are not in `sce`
are left out. It stops if no gene matches.

## See also

Other results:
[`queryCoeff()`](https://spriyansh.github.io/scMaSigPro/reference/queryCoeff.md),
[`showCoeff()`](https://spriyansh.github.io/scMaSigPro/reference/showSol.md),
[`showPoly()`](https://spriyansh.github.io/scMaSigPro/reference/showParams.md)

## Author

Priyansh Srivastava <spriyansh29@gmail.com>

## Examples

``` r
data("scmp.ob", package = "scMaSigPro")
res <- scmpResults(scmp.ob)
head(res)
#> DataFrame with 6 rows and 9 columns
#>           p_value adj_p_value  non_flat model_p_value r_squared influential
#>         <numeric>   <numeric> <logical>     <numeric> <numeric>   <logical>
#> Gene1 2.21646e-01 3.46322e-01     FALSE            NA        NA       FALSE
#> Gene2 1.30835e-13 4.84576e-13      TRUE   1.60139e-09  0.726015       FALSE
#> Gene3 2.51249e-24 1.14204e-23      TRUE   3.98589e-20  0.873163       FALSE
#> Gene4 1.96112e-56 1.96112e-55      TRUE   1.23046e-66  0.955362       FALSE
#> Gene5 6.16240e-02 1.04447e-01     FALSE            NA        NA       FALSE
#> Gene6 5.80848e-01 7.21780e-01     FALSE            NA        NA       FALSE
#>       sig_Path2vsPath1 sig_Path1   cluster
#>              <logical> <logical> <integer>
#> Gene1            FALSE     FALSE        NA
#> Gene2             TRUE     FALSE         1
#> Gene3             TRUE      TRUE         2
#> Gene4             TRUE     FALSE         2
#> Gene5            FALSE     FALSE        NA
#> Gene6            FALSE     FALSE        NA
# The genes of the "Path1" list, ordered by R-squared
sig <- res[res$sig_Path1, ]
head(sig[order(sig$r_squared, decreasing = TRUE), ])
#> DataFrame with 6 rows and 9 columns
#>             p_value  adj_p_value  non_flat model_p_value r_squared influential
#>           <numeric>    <numeric> <logical>     <numeric> <numeric>   <logical>
#> Gene10 8.34110e-248 8.34110e-246      TRUE  7.18412e-269  0.989436       FALSE
#> Gene51 5.22783e-176 2.61392e-174      TRUE  5.22783e-176  0.985444       FALSE
#> Gene18  4.03251e-90  8.06502e-89      TRUE   1.78301e-94  0.971161       FALSE
#> Gene86  1.35821e-90  3.39552e-89      TRUE   9.68258e-97  0.970970       FALSE
#> Gene46  1.63491e-80  2.33559e-79      TRUE   1.14734e-86  0.968460       FALSE
#> Gene72  5.81817e-83  9.69695e-82      TRUE   2.55237e-69  0.961039       FALSE
#>        sig_Path2vsPath1 sig_Path1   cluster
#>               <logical> <logical> <integer>
#> Gene10             TRUE      TRUE         2
#> Gene51             TRUE      TRUE         2
#> Gene18             TRUE      TRUE         3
#> Gene86             TRUE      TRUE         3
#> Gene46             TRUE      TRUE         3
#> Gene72             TRUE      TRUE         2

# scmp.ob was made from the bundled splat.sim data set
data("splat.sim", package = "scMaSigPro")
sce <- scmpToRowData(splat.sim, scmp.ob)
head(SummarizedExperiment::rowData(sce)[, c("Gene", "scmp_p_value")])
#> DataFrame with 6 rows and 2 columns
#>              Gene scmp_p_value
#>       <character>    <numeric>
#> Gene1       Gene1  2.21646e-01
#> Gene2       Gene2  1.30835e-13
#> Gene3       Gene3  2.51249e-24
#> Gene4       Gene4  1.96112e-56
#> Gene5       Gene5  6.16240e-02
#> Gene6       Gene6  5.80848e-01
```
