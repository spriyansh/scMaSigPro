# Cluster the significant genes

`sc.cluster.trend()` clusters the genes of the gene lists of
[`sc.filter()`](https://spriyansh.github.io/scMaSigPro/reference/sc.filter.md)
by the coefficients of their selected models or by their pseudo-bulk
counts, as `see.genes()` of maSigPro does, with hierarchical clustering,
k-means or Gaussian mixture models (mclust).

## Usage

``` r
sc.cluster.trend(
  scmpObj,
  geneSet = "intersect",
  cluster_by = "coeff",
  cluster_method = "hclust",
  hclust.agglo.method = "ward.D",
  kmeans.iter.max = 500,
  hclust.distance = "cor",
  includeInflu = TRUE,
  mclust.k = FALSE,
  k = 9,
  fill_na = "zero",
  use_dim = "col"
)
```

## Arguments

- scmpObj:

  A
  [ScMaSigPro](https://spriyansh.github.io/scMaSigPro/reference/ScMaSigPro-class.md)
  object.

- geneSet:

  The genes to cluster: the name of a list of
  [`sc.filter()`](https://spriyansh.github.io/scMaSigPro/reference/sc.filter.md),
  `"intersect"`, the genes in every list, or `"union"`, the genes in any
  list.

- cluster_by:

  What to cluster: the coefficients of the selected models (`"coeff"`)
  or the pseudo-bulk counts (`"counts"`).

- cluster_method:

  `"hclust"`, `"kmeans"` or `"Mclust"`, which needs the suggested
  package mclust.

- hclust.agglo.method:

  The agglomeration method of
  [`stats::hclust()`](https://rdrr.io/r/stats/hclust.html).

- kmeans.iter.max:

  The maximum number of iterations of
  [`stats::kmeans()`](https://rdrr.io/r/stats/kmeans.html).

- hclust.distance:

  The distance of `"hclust"`: `"cor"`, one minus the correlation of two
  genes, or a method of
  [`stats::dist()`](https://rdrr.io/r/stats/dist.html).

- includeInflu:

  Whether to keep the genes with influential observations (see
  [`showInflu()`](https://spriyansh.github.io/scMaSigPro/reference/showSol.md)).

- mclust.k:

  Whether `"Mclust"` chooses the number of clusters instead of taking
  `k`.

- k:

  The number of clusters.

- fill_na:

  How missing values are filled before clustering: with `"zero"`, or the
  `"mean"` or `"median"` of their column or row (`use_dim`).

- use_dim:

  Whether `fill_na` takes the mean or median by column (`"col"`) or by
  row (`"row"`).

## Value

The object with the cluster of each gene in the `Significant` slot
(class
[Significant](https://spriyansh.github.io/scMaSigPro/reference/Significant-class.md)).

## References

Conesa A, Nueda MJ, Ferrer A, Talón M (2006). maSigPro: a method to
identify significantly differential expression profiles in time-course
microarray experiments. Bioinformatics 22(9), 1096-1102.
[doi:10.1093/bioinformatics/btl056](https://doi.org/10.1093/bioinformatics/btl056)

Scrucca L, Fop M, Murphy TB, Raftery AE (2016). mclust 5: clustering,
classification and density estimation using Gaussian finite mixture
models. The R Journal 8(1), 289-317.
[doi:10.32614/RJ-2016-021](https://doi.org/10.32614/RJ-2016-021)

## See also

[`plotTrendCluster()`](https://spriyansh.github.io/scMaSigPro/reference/plotTrendCluster.md),
which plots the clusters.

Other workflow steps:
[`sc.filter()`](https://spriyansh.github.io/scMaSigPro/reference/sc.filter.md),
[`sc.p.vector()`](https://spriyansh.github.io/scMaSigPro/reference/sc.p.vector.md),
[`sc.restruct()`](https://spriyansh.github.io/scMaSigPro/reference/sc.restruct.md),
[`sc.set.poly()`](https://spriyansh.github.io/scMaSigPro/reference/sc.set.poly.md),
[`sc.squeeze()`](https://spriyansh.github.io/scMaSigPro/reference/sc.squeeze.md),
[`sc.t.fit()`](https://spriyansh.github.io/scMaSigPro/reference/sc.t.fit.md)

## Author

Priyansh Srivastava <spriyansh29@gmail.com>, Ana Conesa and Maria Jose
Nueda, <mj.nueda@ua.es>

## Examples

``` r
data("scmp.ob", package = "scMaSigPro")
# Cluster the pseudo-bulk counts of the genes of every list
scmp <- sc.cluster.trend(scmp.ob,
    geneSet = "union", cluster_by = "counts", k = 3
)
table(scmpResults(scmp)$cluster)
#> 
#>  1  2  3 
#> 12 12 11 
```
