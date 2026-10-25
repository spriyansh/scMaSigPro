# A complete analysis of splat.sim

A
[ScMaSigPro](https://spriyansh.github.io/scMaSigPro/reference/ScMaSigPro-class.md)
object with every step of the workflow run on
[splat.sim](https://spriyansh.github.io/scMaSigPro/reference/splat.sim.md),
for the examples and the vignettes. The [Quick
start](https://spriyansh.github.io/scMaSigPro/articles/scMaSigPro.html)
vignette runs the same steps.

## Usage

``` r
data("scmp.ob", package = "scMaSigPro")
```

## Format

A
[ScMaSigPro](https://spriyansh.github.io/scMaSigPro/reference/ScMaSigPro-class.md)
object:

- `Sparse`:

  The 200 cells and 100 genes of
  [splat.sim](https://spriyansh.github.io/scMaSigPro/reference/splat.sim.md),
  with the pseudotime in `Pseudotime` (`Step` of
  [splat.sim](https://spriyansh.github.io/scMaSigPro/reference/splat.sim.md))
  and the path in `Path` (`Group`).

- `Dense`:

  The 16 bins of
  [`sc.squeeze()`](https://spriyansh.github.io/scMaSigPro/reference/sc.squeeze.md)
  with its defaults, 8 per path, and their pseudo-bulk counts.

- `Design`:

  The design of
  [`sc.set.poly()`](https://spriyansh.github.io/scMaSigPro/reference/sc.set.poly.md)
  with `poly_degree = 1` and the offsets of
  [`sc.p.vector()`](https://spriyansh.github.io/scMaSigPro/reference/sc.p.vector.md).

- `Profile`:

  The test of the full model of every gene by
  [`sc.p.vector()`](https://spriyansh.github.io/scMaSigPro/reference/sc.p.vector.md):
  negative binomial family with theta 10, log link and size-factor
  offsets.

- `Estimate`:

  The models selected by
  [`sc.t.fit()`](https://spriyansh.github.io/scMaSigPro/reference/sc.t.fit.md)
  (backward selection).

- `Significant`:

  The gene lists `Path1` and `Path2vsPath1` of
  [`sc.filter()`](https://spriyansh.github.io/scMaSigPro/reference/sc.filter.md)
  with `rsq = 0.7` and `vars = "groups"`, and four clusters of their
  union from
  [`sc.cluster.trend()`](https://spriyansh.github.io/scMaSigPro/reference/sc.cluster.trend.md).

- `Parameters`:

  The settings of these steps.

## Source

Made by
`system.file("scripts", "make-scmp-ob.R", package = "scMaSigPro")` from
[splat.sim](https://spriyansh.github.io/scMaSigPro/reference/splat.sim.md).

## References

Srivastava P, Benegas Coll M, Götz S, Nueda MJ, Conesa A (2024).
scMaSigPro: differential expression analysis along single-cell
trajectories. Bioinformatics 40(7), btae443.
[doi:10.1093/bioinformatics/btae443](https://doi.org/10.1093/bioinformatics/btae443)

## See also

Other datasets:
[`multi.lin.sce`](https://spriyansh.github.io/scMaSigPro/reference/multi.lin.sce.md),
[`splat.sim`](https://spriyansh.github.io/scMaSigPro/reference/splat.sim.md)

## Author

Priyansh Srivastava <spriyansh29@gmail.com>

## Examples

``` r
data("scmp.ob", package = "scMaSigPro")
scmp.ob
#> Class: ScMaSigProClass
#> nCells: 200
#> nFeatures: 100
#> Pseudotime Range: 1 100
#> Branching Paths: Path2, Path1
#> Binned Pseudotime: 1-8(Range), 4.5(Mean), 
#> Number of bins-> Path1: 8 Path2: 8
#> Average bin Size-> Path1: 12 Path2: 13
#> Polynomial Order: 1
#> No. of Significant Profiles: 50
#> No. of Influential Features: 8
```
