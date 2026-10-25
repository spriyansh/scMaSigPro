# Plot the trends of the clusters

`plotTrendCluster()` plots, in one panel per cluster of
[`sc.cluster.trend()`](https://spriyansh.github.io/scMaSigPro/reference/sc.cluster.trend.md),
the points of its genes along the binned pseudotime and a loess curve
per path through their fitted curves.

## Usage

``` r
plotTrendCluster(
  scmpObj,
  xlab = "Pooled Pseudotime",
  ylab = NULL,
  plot = "counts",
  summary_mode = "median",
  logs = NULL,
  logType = "log",
  smoothness = 1,
  includeInflu = TRUE,
  verbose = TRUE,
  pseudoCount = 1,
  significant = FALSE,
  curves = TRUE,
  lines = FALSE,
  points = TRUE,
  loess_span = 0.8
)
```

## Arguments

- scmpObj:

  A
  [ScMaSigPro](https://spriyansh.github.io/scMaSigPro/reference/ScMaSigPro-class.md)
  object.

- xlab:

  The label of the x axis.

- ylab:

  The label of the y axis. The default, `NULL`, is
  `"log(Pseudobulk Expression)"` for log-link fits and when the points
  are logged, and `"Pseudobulk Expression"` otherwise.

- plot:

  Not used; it must be `"counts"` or `"coeff"`.

- summary_mode:

  How the points of each binned pseudotime and path are summarised for
  the lines: `"median"` or `"mean"`.

- logs:

  Whether to log the points, as in
  [`plotTrend()`](https://spriyansh.github.io/scMaSigPro/reference/plotTrend.md).
  The default, `NULL`, logs them when the fits used the log link.

- logType:

  The log of the points: `"log"`, `"log2"` or `"log10"`.

- smoothness:

  The step of the pseudotime grid of the curves: the smaller, the
  smoother.

- includeInflu:

  Not used.

- verbose:

  Whether to print progress messages.

- pseudoCount:

  The number added to the points before the log.

- significant:

  Whether the genes must be in a gene list of
  [`sc.filter()`](https://spriyansh.github.io/scMaSigPro/reference/sc.filter.md);
  the others are left out with a warning.

- curves:

  Whether to draw the fitted curves.

- lines:

  Whether to draw lines through the points.

- points:

  Whether to draw the points.

- loess_span:

  The span of the loess curves.

## Value

A ggplot object with one panel per cluster.

## See also

Other plots:
[`plotBinTile()`](https://spriyansh.github.io/scMaSigPro/reference/plotBinTile.md),
[`plotDiagnostics()`](https://spriyansh.github.io/scMaSigPro/reference/plotDiagnostics.md),
[`plotIntersect()`](https://spriyansh.github.io/scMaSigPro/reference/plotIntersect.md),
[`plotTrend()`](https://spriyansh.github.io/scMaSigPro/reference/plotTrend.md)

## Author

Priyansh Srivastava <spriyansh29@gmail.com>

## Examples

``` r
data("scmp.ob", package = "scMaSigPro")
plotTrendCluster(scmp.ob, verbose = FALSE)

```
