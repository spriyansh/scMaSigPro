# Test the full model of each gene

`sc.p.vector()` fits the full polynomial model of each gene to its
pseudo-bulk counts and tests it against the intercept-only model. The
genes whose adjusted p-value is at most `p_value` have a non-flat
profile;
[`sc.t.fit()`](https://spriyansh.github.io/scMaSigPro/reference/sc.t.fit.md)
selects the terms of their models.

## Usage

``` r
sc.p.vector(
  scmpObj,
  p_value = 0.05,
  mt_correction = "BH",
  min_na = 6,
  min_counts = 0,
  min_bins_expressed = 0,
  family = negative.binomial(theta = 10, link = link),
  epsilon = 1e-08,
  verbose = TRUE,
  offset = TRUE,
  max_it = 100,
  link = "log",
  theta_estimate = "none",
  test = "chisq",
  BPPARAM = SerialParam()
)
```

## Arguments

- scmpObj:

  A
  [ScMaSigPro](https://spriyansh.github.io/scMaSigPro/reference/ScMaSigPro-class.md)
  object.

- p_value:

  The level of the adjusted p-values: the genes at or below it have a
  non-flat profile. It is also the default level of
  [`sc.t.fit()`](https://spriyansh.github.io/scMaSigPro/reference/sc.t.fit.md).

- mt_correction:

  The method of
  [`stats::p.adjust()`](https://rdrr.io/r/stats/p.adjust.html) that
  adjusts the p-values for multiple testing; `"BH"` is that of Benjamini
  and Hochberg (1995).

- min_na:

  The minimum number of bins with a value (not `NA`) that a gene needs
  to be fitted, as `min.obs` of `p.vector()` in maSigPro. Only counts
  with missing values, such as microarray data given to
  [`create_scmp()`](https://spriyansh.github.io/scMaSigPro/reference/create_scmp.md)
  with `use_as_bin = TRUE`, have fewer; it can be at most the number of
  bins of the design.

- min_counts:

  Minimum total count of a gene, summed over the bins of the design.
  Genes with fewer counts are left out of the fit. The default, 0, keeps
  every gene.

- min_bins_expressed:

  Minimum number of bins of the design in which a gene has a count
  above 0. Genes expressed in fewer bins are left out of the fit. The
  default, 0, keeps every gene; it can be at most the number of bins.

- family:

  The distribution of the error term, a
  [`stats::family()`](https://rdrr.io/r/stats/family.html) object: by
  default the negative binomial with `theta = 10` and the link `link`. A
  family given here keeps its own link.

- epsilon:

  The convergence tolerance of
  [`stats::glm()`](https://rdrr.io/r/stats/glm.html).

- verbose:

  Whether to print progress messages.

- offset:

  Whether to add the offset of each bin, the logarithm of its size
  factor (see Details), to the linear predictor; it needs the log link.

- max_it:

  The maximum number of iterations of
  [`stats::glm()`](https://rdrr.io/r/stats/glm.html).

- link:

  The link of the default family: `"log"`, the model of the paper, or
  `"identity"`. When `family` is given, `link` must be its link. The
  identity link needs `offset = FALSE`; under it the fitted mean of a
  gene can become negative, and
  [`stats::glm()`](https://rdrr.io/r/stats/glm.html) then warns.

- theta_estimate:

  For a negative binomial `family`: `"none"` (the default) fits every
  gene with the theta of `family` (10 by default, as maSigPro). `"gene"`
  estimates theta by maximum likelihood on each gene's full model and
  fits the gene with it; `"global"` fits every gene with the median of
  these estimates. The estimates are kept in the `distribution`
  parameter, for
  [`sc.t.fit()`](https://spriyansh.github.io/scMaSigPro/reference/sc.t.fit.md)
  and
  [`plotDiagnostics()`](https://spriyansh.github.io/scMaSigPro/reference/plotDiagnostics.md):
  `theta_gene` holds one per gene and `theta_estimate` the choice.

- test:

  The test of each gene's full model against its intercept-only model:
  `"chisq"` (the default) or `"F"`. Both divide the difference in
  deviance between the two models by the dispersion of the full model,
  which, for the negative binomial and Gaussian families, is estimated
  as its Pearson statistic divided by its residual degrees of freedom.
  `"chisq"` compares the result with the chi-squared distribution, as
  maSigPro does. `"F"` also divides it by the difference in degrees of
  freedom and compares it with the F distribution on the residual
  degrees of freedom of the full model: the F test of
  [`stats::anova.glm()`](https://rdrr.io/r/stats/anova.glm.html). With
  the Gaussian family both values give the F test, the test of maSigPro
  for that family. `"F"` needs a family whose dispersion is estimated,
  so not [`stats::binomial()`](https://rdrr.io/r/stats/family.html) or
  [`stats::poisson()`](https://rdrr.io/r/stats/family.html). The value
  is kept in the `test` parameter. See Details.

- BPPARAM:

  A `BiocParallelParam` object that runs the per-gene fits, such as
  `BiocParallel::MulticoreParam(2)` (Linux and macOS) or
  `BiocParallel::SnowParam(2)` (any system). The default,
  [`BiocParallel::SerialParam()`](https://rdrr.io/pkg/BiocParallel/man/SerialParam-class.html),
  fits the genes one after another; create it with `progressbar = TRUE`
  for a progress bar. The results do not depend on the backend.

## Value

The object with the p-values and the genes with a non-flat profile in
the `Profile` slot (class
[VariableProfiles](https://spriyansh.github.io/scMaSigPro/reference/VariableProfiles-class.md)),
the offsets in the `Design` slot and the settings of the fit in the
`Parameters` slot. The `Estimate` and `Significant` slots, computed from
earlier tests, are emptied.

## Details

With `offset = TRUE`, the fits include an offset as Srivastava et al.
(2024), Section 2.3 and Supplementary Section S1.3, describe it:
"scMaSigPro incorporates an offset value to the Poly-GLM model to adjust
for disparities in total read counts per bin. This offset is calculated
as the logarithm of size factors for each bin". The size factors are
those of the median-of-ratios method (Anders and Huber 2010; Love et al.
2014) on the raw counts: "The size factor for each bin is the median of
the ratios of its counts to the geometric means for each gene". Only the
genes with a count above 0 in every bin enter the medians, and the fit
stops when there is none: fewer, larger bins
([`sc.squeeze()`](https://spriyansh.github.io/scMaSigPro/reference/sc.squeeze.md))
or `offset = FALSE` avoid it. The genes are fitted on their raw counts,
without a pseudo-count: "scMaSigPro can directly process raw counts".
The offsets are stored in the `Design` slot, where
[`sc.t.fit()`](https://spriyansh.github.io/scMaSigPro/reference/sc.t.fit.md)
finds them.

Genes are left out of the fit when they have fewer than `min_na` values,
when all their counts are zero, and when they are below `min_counts` or
`min_bins_expressed`. They have no p-value and do not count in the
multiple-testing correction;
[`scmpResults()`](https://spriyansh.github.io/scMaSigPro/reference/scmpResults.md)
gives them `NA`. The size factors are computed before `min_counts` and
`min_bins_expressed` are applied, so these two filters change the
results of the genes they keep only through the correction.

The test of the full model is the first step of the method of Srivastava
et al. (2024), Section 2.4: "scMaSigPro implements a two-step strategy
similar to the maSigPro method", in which the full model of each gene is
"evaluated against a simpler intercept-only model via hypothesis testing
based on the log-likelihood ratio statistic". The default,
`test = "chisq"`, is that test as maSigPro computes it for families
other than the Gaussian: the statistic is divided by the estimated
dispersion and compared with the chi-squared distribution, as if the
dispersion were known. With few bins, and so few residual degrees of
freedom, the estimate varies from gene to gene and the test calls more
genes significant than its level. `test = "F"`, the quasi-likelihood F
test, accounts for the estimation of the dispersion through the residual
degrees of freedom of the F distribution.

## References

Srivastava P, Benegas Coll M, Götz S, Nueda MJ, Conesa A (2024).
scMaSigPro: differential expression analysis along single-cell
trajectories. Bioinformatics 40(7), btae443.
[doi:10.1093/bioinformatics/btae443](https://doi.org/10.1093/bioinformatics/btae443)

Conesa A, Nueda MJ, Ferrer A, Talón M (2006). maSigPro: a method to
identify significantly differential expression profiles in time-course
microarray experiments. Bioinformatics 22(9), 1096-1102.
[doi:10.1093/bioinformatics/btl056](https://doi.org/10.1093/bioinformatics/btl056)

Nueda MJ, Tarazona S, Conesa A (2014). Next maSigPro: updating maSigPro
bioconductor package for RNA-seq time series. Bioinformatics 30(18),
2598-2602.
[doi:10.1093/bioinformatics/btu333](https://doi.org/10.1093/bioinformatics/btu333)

Anders S, Huber W (2010). Differential expression analysis for sequence
count data. Genome Biology 11(10), R106.
[doi:10.1186/gb-2010-11-10-r106](https://doi.org/10.1186/gb-2010-11-10-r106)

Love MI, Huber W, Anders S (2014). Moderated estimation of fold change
and dispersion for RNA-seq data with DESeq2. Genome Biology 15(12), 550.
[doi:10.1186/s13059-014-0550-8](https://doi.org/10.1186/s13059-014-0550-8)

Benjamini Y, Hochberg Y (1995). Controlling the false discovery rate: a
practical and powerful approach to multiple testing. Journal of the
Royal Statistical Society: Series B 57(1), 289-300.
[doi:10.1111/j.2517-6161.1995.tb02031.x](https://doi.org/10.1111/j.2517-6161.1995.tb02031.x)

Venables WN, Ripley BD (2002). Modern Applied Statistics with S, fourth
edition. Springer, New York. ISBN 0-387-95457-0.

## See also

[`scmpResults()`](https://spriyansh.github.io/scMaSigPro/reference/scmpResults.md)
for the p-values;
[VariableProfiles](https://spriyansh.github.io/scMaSigPro/reference/VariableProfiles-class.md).

Other workflow steps:
[`sc.cluster.trend()`](https://spriyansh.github.io/scMaSigPro/reference/sc.cluster.trend.md),
[`sc.filter()`](https://spriyansh.github.io/scMaSigPro/reference/sc.filter.md),
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
# Test the full model of every gene with the F test
scmp <- sc.p.vector(scmp.ob, test = "F", verbose = FALSE)
sum(scmpResults(scmp)$non_flat)
#> [1] 43
```
