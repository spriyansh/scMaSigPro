# Plot the sizes of the bins

`plotBinTile()` draws a tile for each bin, by path and binned
pseudotime, coloured and labelled by its number of cells.

## Usage

``` r
plotBinTile(
  scmpObj,
  path_col = scmpObj@Parameters@path_col,
  bin_size_col = scmpObj@Parameters@bin_size_col,
  bin_ptime_col = scmpObj@Parameters@bin_ptime_col
)
```

## Arguments

- scmpObj:

  A
  [ScMaSigPro](https://spriyansh.github.io/scMaSigPro/reference/ScMaSigPro-class.md)
  object.

- path_col:

  The name of the column of the cell or bin metadata that holds the path
  label of each cell or bin.

- bin_size_col:

  The name of the column of the bin metadata that holds the number of
  cells of each bin.

- bin_ptime_col:

  The name of the column of the bin metadata that holds the binned
  pseudotime of each bin.

## Value

A ggplot object.

## See also

Other plots:
[`plotDiagnostics()`](https://spriyansh.github.io/scMaSigPro/reference/plotDiagnostics.md),
[`plotIntersect()`](https://spriyansh.github.io/scMaSigPro/reference/plotIntersect.md),
[`plotTrend()`](https://spriyansh.github.io/scMaSigPro/reference/plotTrend.md),
[`plotTrendCluster()`](https://spriyansh.github.io/scMaSigPro/reference/plotTrendCluster.md)

## Author

Priyansh Srivastava <spriyansh29@gmail.com>

## Examples

``` r
data("scmp.ob", package = "scMaSigPro")
plotBinTile(scmp.ob)

```
