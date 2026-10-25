# Select the significant terms of each gene

`sc.t.fit()` selects by stepwise regression, as maSigPro does, the
significant terms of the full model of each gene with a non-flat profile
([`sc.p.vector()`](https://spriyansh.github.io/scMaSigPro/reference/sc.p.vector.md)),
and keeps the coefficients, p-values and t statistics of the selected
models.

## Usage

``` r
sc.t.fit(
  scmpObj,
  selection_method = "backward",
  p_value = scmpObj@Parameters@p_value,
  nvar_correction = FALSE,
  family = scmpObj@Parameters@distribution,
  epsilon = scmpObj@Parameters@epsilon,
  offset = scmpObj@Parameters@offset,
  verbose = TRUE,
  max_it = scmpObj@Parameters@max_it,
  link = scmpObj@Parameters@link,
  BPPARAM = SerialParam()
)
```

## Arguments

- scmpObj:

  A
  [ScMaSigPro](https://spriyansh.github.io/scMaSigPro/reference/ScMaSigPro-class.md)
  object.

- selection_method:

  The stepwise method: `"backward"`, `"forward"`, `"two.ways.backward"`
  or `"two.ways.forward"`.

- p_value:

  The level at which a term enters or stays in a model; by default that
  of
  [`sc.p.vector()`](https://spriyansh.github.io/scMaSigPro/reference/sc.p.vector.md).

- nvar_correction:

  Whether to divide `p_value` by the number of terms of the design.

- family:

  The distribution of the error term, a
  [`stats::family()`](https://rdrr.io/r/stats/family.html) object; by
  default the family of
  [`sc.p.vector()`](https://spriyansh.github.io/scMaSigPro/reference/sc.p.vector.md),
  with its link. When
  [`sc.p.vector()`](https://spriyansh.github.io/scMaSigPro/reference/sc.p.vector.md)
  estimated one theta per gene (`theta_estimate = "gene"`), each gene is
  fitted with its own theta.

- epsilon:

  The convergence tolerance of
  [`stats::glm()`](https://rdrr.io/r/stats/glm.html).

- offset:

  Whether to add the offset of each bin, the logarithm of its size
  factor, which needs the log link. The fits use the offsets that
  [`sc.p.vector()`](https://spriyansh.github.io/scMaSigPro/reference/sc.p.vector.md)
  stored in the `Design` slot; when it stored none (`offset = FALSE`),
  they are computed here in the same way.

- verbose:

  Whether to print progress messages.

- max_it:

  The maximum number of iterations of
  [`stats::glm()`](https://rdrr.io/r/stats/glm.html).

- link:

  The link of `family`, by default that of
  [`sc.p.vector()`](https://spriyansh.github.io/scMaSigPro/reference/sc.p.vector.md);
  when given, it must be the link of `family`. Choose the link in
  [`sc.p.vector()`](https://spriyansh.github.io/scMaSigPro/reference/sc.p.vector.md).

- BPPARAM:

  A `BiocParallelParam` object that runs the per-gene fits, such as
  `BiocParallel::MulticoreParam(2)` (Linux and macOS) or
  `BiocParallel::SnowParam(2)` (any system). The default,
  [`BiocParallel::SerialParam()`](https://rdrr.io/pkg/BiocParallel/man/SerialParam-class.html),
  fits the genes one after another; create it with `progressbar = TRUE`
  for a progress bar. The results do not depend on the backend.

## Value

The object with the selected model of each gene with a non-flat profile
in the `Estimate` slot (class
[Estimates](https://spriyansh.github.io/scMaSigPro/reference/Estimates-class.md)),
read by
[`showSol()`](https://spriyansh.github.io/scMaSigPro/reference/showSol.md)
and the other result readers.

## References

Conesa A, Nueda MJ, Ferrer A, Talón M (2006). maSigPro: a method to
identify significantly differential expression profiles in time-course
microarray experiments. Bioinformatics 22(9), 1096-1102.
[doi:10.1093/bioinformatics/btl056](https://doi.org/10.1093/bioinformatics/btl056)

Nueda MJ, Tarazona S, Conesa A (2014). Next maSigPro: updating maSigPro
bioconductor package for RNA-seq time series. Bioinformatics 30(18),
2598-2602.
[doi:10.1093/bioinformatics/btu333](https://doi.org/10.1093/bioinformatics/btu333)

Srivastava P, Benegas Coll M, Götz S, Nueda MJ, Conesa A (2024).
scMaSigPro: differential expression analysis along single-cell
trajectories. Bioinformatics 40(7), btae443.
[doi:10.1093/bioinformatics/btae443](https://doi.org/10.1093/bioinformatics/btae443)

## See also

[`showSol()`](https://spriyansh.github.io/scMaSigPro/reference/showSol.md)
and the other readers of the results;
[Estimates](https://spriyansh.github.io/scMaSigPro/reference/Estimates-class.md).

Other workflow steps:
[`sc.cluster.trend()`](https://spriyansh.github.io/scMaSigPro/reference/sc.cluster.trend.md),
[`sc.filter()`](https://spriyansh.github.io/scMaSigPro/reference/sc.filter.md),
[`sc.p.vector()`](https://spriyansh.github.io/scMaSigPro/reference/sc.p.vector.md),
[`sc.restruct()`](https://spriyansh.github.io/scMaSigPro/reference/sc.restruct.md),
[`sc.set.poly()`](https://spriyansh.github.io/scMaSigPro/reference/sc.set.poly.md),
[`sc.squeeze()`](https://spriyansh.github.io/scMaSigPro/reference/sc.squeeze.md)

## Author

Priyansh Srivastava <spriyansh29@gmail.com>, Ana Conesa and Maria Jose
Nueda, <mj.nueda@ua.es>

## Examples

``` r
data("scmp.ob", package = "scMaSigPro")
# Keep the terms with a p-value below 0.01
scmp <- sc.t.fit(scmp.ob, p_value = 0.01, verbose = FALSE)
head(showSol(scmp)[, c("p-value", "R-squared")])
#>              p-value    R-squared
#> Gene2   1.601390e-09 7.260152e-01
#> Gene3   3.985890e-20 8.731628e-01
#> Gene4   1.230465e-66 9.553623e-01
#> Gene9   5.443935e-27 9.012376e-01
#> Gene10 7.184117e-269 9.894360e-01
#> Gene11            NA 3.573648e-12
```
