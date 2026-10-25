# VariableProfiles

The test of the full model of each gene by
[`sc.p.vector()`](https://spriyansh.github.io/scMaSigPro/reference/sc.p.vector.md).

## Value

An object of class `VariableProfiles`: the test of the full model of
each gene, which
[`sc.p.vector()`](https://spriyansh.github.io/scMaSigPro/reference/sc.p.vector.md)
stores in the `Profile` slot of a
[ScMaSigPro](https://spriyansh.github.io/scMaSigPro/reference/ScMaSigPro-class.md)
object.
[`scmpResults()`](https://spriyansh.github.io/scMaSigPro/reference/scmpResults.md)
returns its p-values.

## Details

`validObject()` checks that `p_values` and `adj_p_values` have the same
length and names, that `fdr` holds at most one value and that every gene
in `non_flat` is named in `p_values`.

## Slots

- `non_flat`:

  The genes with a non-flat profile.

- `p_values`:

  The p-value of every fitted gene, named by gene.

- `adj_p_values`:

  The adjusted p-values, in the same order.

- `fdr`:

  The largest p-value of the genes with a non-flat profile, empty when
  there is none.

## See also

Other classes:
[`Estimates`](https://spriyansh.github.io/scMaSigPro/reference/Estimates-class.md),
[`MatrixDesign`](https://spriyansh.github.io/scMaSigPro/reference/MatrixDesign-class.md),
[`ParameterConfig`](https://spriyansh.github.io/scMaSigPro/reference/ParameterConfig-class.md),
[`ScMaSigPro`](https://spriyansh.github.io/scMaSigPro/reference/ScMaSigPro-class.md),
[`Significant`](https://spriyansh.github.io/scMaSigPro/reference/Significant-class.md),
[`updateObject,ScMaSigPro-method`](https://spriyansh.github.io/scMaSigPro/reference/updateObject-ScMaSigPro-method.md)

## Author

Priyansh Srivastava <spriyansh29@gmail.com>

## Examples

``` r
data("scmp.ob", package = "scMaSigPro")
res <- scmpResults(scmp.ob)
head(res[, c("p_value", "adj_p_value", "non_flat")])
#> DataFrame with 6 rows and 3 columns
#>           p_value adj_p_value  non_flat
#>         <numeric>   <numeric> <logical>
#> Gene1 2.21646e-01 3.46322e-01     FALSE
#> Gene2 1.30835e-13 4.84576e-13      TRUE
#> Gene3 2.51249e-24 1.14204e-23      TRUE
#> Gene4 1.96112e-56 1.96112e-55      TRUE
#> Gene5 6.16240e-02 1.04447e-01     FALSE
#> Gene6 5.80848e-01 7.21780e-01     FALSE
```
