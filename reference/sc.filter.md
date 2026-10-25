# List the significant genes

`sc.filter()` lists the genes whose selected model
([`sc.t.fit()`](https://spriyansh.github.io/scMaSigPro/reference/sc.t.fit.md))
has an R-squared of at least `rsq` and a p-value of at most `p_value`:
in one list, one list per term or one list per path (`vars`), as
`get.siggenes()` of maSigPro does.

## Usage

``` r
sc.filter(
  scmpObj,
  rsq = 0.7,
  p_value = scmpObj@Parameters@p_value,
  vars = c("all", "each", "groups"),
  intercept = "dummy",
  term_p_value = 0.05,
  includeInflu = TRUE
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

- vars:

  `"all"`: one list with every gene that passes; `"each"`: one list per
  term, with the genes whose term passes `term_p_value`; `"groups"`: one
  list per path, with the genes with a term of that path that passes it.

- intercept:

  With `vars = "groups"`, the intercept terms that count: `"dummy"`, the
  dummies of the paths (the differences of their intercepts from the
  reference); `"all"`, these and the intercept of the reference path;
  `"none"`, neither (with more than two paths, only the dummy of the
  first path after the reference is left out).

- term_p_value:

  The largest p-value of a term, for `vars = "each"` and
  `vars = "groups"`.

- includeInflu:

  Whether to keep the genes with influential observations (see
  [`showInflu()`](https://spriyansh.github.io/scMaSigPro/reference/showSol.md)).

## Value

The object with the gene lists in the `Significant` slot (class
[Significant](https://spriyansh.github.io/scMaSigPro/reference/Significant-class.md));
earlier clusters are removed.

## References

Conesa A, Nueda MJ, Ferrer A, Talón M (2006). maSigPro: a method to
identify significantly differential expression profiles in time-course
microarray experiments. Bioinformatics 22(9), 1096-1102.
[doi:10.1093/bioinformatics/btl056](https://doi.org/10.1093/bioinformatics/btl056)

Nueda MJ, Tarazona S, Conesa A (2014). Next maSigPro: updating maSigPro
bioconductor package for RNA-seq time series. Bioinformatics 30(18),
2598-2602.
[doi:10.1093/bioinformatics/btu333](https://doi.org/10.1093/bioinformatics/btu333)

## See also

[`plotIntersect()`](https://spriyansh.github.io/scMaSigPro/reference/plotIntersect.md)
for the overlaps of the lists;
[Significant](https://spriyansh.github.io/scMaSigPro/reference/Significant-class.md).

Other workflow steps:
[`sc.cluster.trend()`](https://spriyansh.github.io/scMaSigPro/reference/sc.cluster.trend.md),
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
scmp <- sc.filter(scmp.ob, rsq = 0.6, vars = "groups")
res <- scmpResults(scmp)
# The number of genes in each list
c(Path1 = sum(res$sig_Path1), Path2vsPath1 = sum(res$sig_Path2vsPath1))
#>        Path1 Path2vsPath1 
#>           25           40 
```
