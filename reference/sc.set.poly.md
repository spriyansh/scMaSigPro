# Set up the polynomial design

`sc.set.poly()` builds the design of the maSigPro model (Conesa et al.
2006): a 0/1 dummy for every path but the first, the reference, and the
products of each dummy with each power of the binned pseudotime up to
`poly_degree`.

The reference is `reference` when it is given. Otherwise it is the first
path of the `Dense` slot: after
[`sc.squeeze()`](https://spriyansh.github.io/scMaSigPro/reference/sc.squeeze.md),
the first level of a factor path column, or else the first label in
C-locale (byte) order; after
[`sc.restruct()`](https://spriyansh.github.io/scMaSigPro/reference/sc.restruct.md),
the first path of `end_node_list`; with bins given to
[`create_scmp()`](https://spriyansh.github.io/scMaSigPro/reference/create_scmp.md)
or
[`cDense()`](https://spriyansh.github.io/scMaSigPro/reference/cDense.md),
the path of the first bin. The other paths keep the order of the `Dense`
slot.

## Usage

``` r
sc.set.poly(
  scmpObj,
  poly_degree = 2,
  bin_ptime_col = scmpObj@Parameters@bin_ptime_col,
  path_col = scmpObj@Parameters@path_col,
  reference = NULL
)
```

## Arguments

- scmpObj:

  A
  [ScMaSigPro](https://spriyansh.github.io/scMaSigPro/reference/ScMaSigPro-class.md)
  object.

- poly_degree:

  The degree of the polynomial in pseudotime.

- bin_ptime_col:

  The name of the column of the bin metadata that holds the binned
  pseudotime of each bin.

- path_col:

  The name of the column of the cell or bin metadata that holds the path
  label of each cell or bin.

- reference:

  The reference path, one of the path labels of the `Dense` slot, or
  `NULL` for the first path.

## Value

The object with the design in the `Design` slot (class
[MatrixDesign](https://spriyansh.github.io/scMaSigPro/reference/MatrixDesign-class.md)).
The `Profile`, `Estimate` and `Significant` slots, computed with an
earlier design, are emptied.

## References

Conesa A, Nueda MJ, Ferrer A, Talón M (2006). maSigPro: a method to
identify significantly differential expression profiles in time-course
microarray experiments. Bioinformatics 22(9), 1096-1102.
[doi:10.1093/bioinformatics/btl056](https://doi.org/10.1093/bioinformatics/btl056)

Srivastava P, Benegas Coll M, Götz S, Nueda MJ, Conesa A (2024).
scMaSigPro: differential expression analysis along single-cell
trajectories. Bioinformatics 40(7), btae443.
[doi:10.1093/bioinformatics/btae443](https://doi.org/10.1093/bioinformatics/btae443)

## See also

[`showPoly()`](https://spriyansh.github.io/scMaSigPro/reference/showParams.md),
[`predictors()`](https://spriyansh.github.io/scMaSigPro/reference/predictors.md)
and
[`pathAssign()`](https://spriyansh.github.io/scMaSigPro/reference/pathAssign.md),
which read the design;
[MatrixDesign](https://spriyansh.github.io/scMaSigPro/reference/MatrixDesign-class.md).

Other workflow steps:
[`sc.cluster.trend()`](https://spriyansh.github.io/scMaSigPro/reference/sc.cluster.trend.md),
[`sc.filter()`](https://spriyansh.github.io/scMaSigPro/reference/sc.filter.md),
[`sc.p.vector()`](https://spriyansh.github.io/scMaSigPro/reference/sc.p.vector.md),
[`sc.restruct()`](https://spriyansh.github.io/scMaSigPro/reference/sc.restruct.md),
[`sc.squeeze()`](https://spriyansh.github.io/scMaSigPro/reference/sc.squeeze.md),
[`sc.t.fit()`](https://spriyansh.github.io/scMaSigPro/reference/sc.t.fit.md)

## Author

Priyansh Srivastava <spriyansh29@gmail.com>, Ana Conesa and Maria Jose
Nueda, <mj.nueda@ua.es>

## Examples

``` r
data("scmp.ob", package = "scMaSigPro")
scmp <- sc.set.poly(scmp.ob, poly_degree = 2)
showPoly(scmp)
#> [1] "beta0 + beta1*Path2vsPath1 + beta2*scmp_binned_pseudotime + beta3*scmp_binned_pseudotimexPath2 + beta4*scmp_binned_pseudotime2 + beta5*scmp_binned_pseudotime2xPath2"
```
