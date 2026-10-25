# Significant

The gene lists of
[`sc.filter()`](https://spriyansh.github.io/scMaSigPro/reference/sc.filter.md)
and the clusters of
[`sc.cluster.trend()`](https://spriyansh.github.io/scMaSigPro/reference/sc.cluster.trend.md).

## Value

An object of class `Significant`: the gene lists of
[`sc.filter()`](https://spriyansh.github.io/scMaSigPro/reference/sc.filter.md)
and the clusters of
[`sc.cluster.trend()`](https://spriyansh.github.io/scMaSigPro/reference/sc.cluster.trend.md),
stored in the `Significant` slot of a
[ScMaSigPro](https://spriyansh.github.io/scMaSigPro/reference/ScMaSigPro-class.md)
object.
[`scmpResults()`](https://spriyansh.github.io/scMaSigPro/reference/scmpResults.md)
returns them per gene.

## Details

`validObject()` checks that every element of `genes` is a character
vector of gene names and that `clusters` holds one value per gene, named
by gene.

## Slots

- `genes`:

  A named list of gene lists.

- `clusters`:

  A list with the cluster of each clustered gene, named by gene.

## See also

Other classes:
[`Estimates`](https://spriyansh.github.io/scMaSigPro/reference/Estimates-class.md),
[`MatrixDesign`](https://spriyansh.github.io/scMaSigPro/reference/MatrixDesign-class.md),
[`ParameterConfig`](https://spriyansh.github.io/scMaSigPro/reference/ParameterConfig-class.md),
[`ScMaSigPro`](https://spriyansh.github.io/scMaSigPro/reference/ScMaSigPro-class.md),
[`VariableProfiles`](https://spriyansh.github.io/scMaSigPro/reference/VariableProfiles-class.md),
[`updateObject,ScMaSigPro-method`](https://spriyansh.github.io/scMaSigPro/reference/updateObject-ScMaSigPro-method.md)

## Examples

``` r
data("scmp.ob", package = "scMaSigPro")
table(scmpResults(scmp.ob)$cluster)
#> 
#>  1  2  3  4 
#> 11 15  8  1 
```
