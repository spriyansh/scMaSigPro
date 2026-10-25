# Plot the diagnostics of the model of a gene

`plotDiagnostics()` draws four diagnostic plots of a model of a gene of
the gene lists of
[`sc.filter()`](https://spriyansh.github.io/scMaSigPro/reference/sc.filter.md):
the normal Q-Q plot of the standardised residuals, the standardised
residuals and their square root against the fitted values
(scale-location), and the observed against the predicted counts.

## Usage

``` r
plotDiagnostics(scmpObj, feature_id, model = "optimized")
```

## Arguments

- scmpObj:

  A
  [ScMaSigPro](https://spriyansh.github.io/scMaSigPro/reference/ScMaSigPro-class.md)
  object.

- feature_id:

  The name of a gene.

- model:

  The model: `"optimized"`, the model selected by
  [`sc.t.fit()`](https://spriyansh.github.io/scMaSigPro/reference/sc.t.fit.md);
  `"full"`; or `"intercept"`, the intercept-only model.

## Value

A patchwork object with four ggplot panels.

## See also

Other plots:
[`plotBinTile()`](https://spriyansh.github.io/scMaSigPro/reference/plotBinTile.md),
[`plotIntersect()`](https://spriyansh.github.io/scMaSigPro/reference/plotIntersect.md),
[`plotTrend()`](https://spriyansh.github.io/scMaSigPro/reference/plotTrend.md),
[`plotTrendCluster()`](https://spriyansh.github.io/scMaSigPro/reference/plotTrendCluster.md)

## Author

Priyansh Srivastava <spriyansh29@gmail.com>

## Examples

``` r
data("scmp.ob", package = "scMaSigPro")
if (requireNamespace("patchwork", quietly = TRUE)) {
    plotDiagnostics(scmp.ob, "Gene10")
}

```
