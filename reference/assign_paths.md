# Assign cells to paths from lineage weights

`assign_paths()` turns the lineage weights and pseudotimes of a
trajectory tool, one column per lineage as slingshot and tradeSeq give
them, into what scMaSigPro needs: one pseudotime value and one path
label per cell. Cells on every lineage are labelled as the root, and
cells on some of the lineages get the names of those lineages joined by
`shared_sep`: the root and link paths that
[`sc.restruct()`](https://spriyansh.github.io/scMaSigPro/reference/sc.restruct.md)
takes.

## Usage

``` r
assign_paths(
  pseudotime,
  weights,
  min_weight = 0.5,
  root_label = "root",
  shared_sep = "_links_",
  verbose = TRUE
)
```

## Arguments

- pseudotime:

  The pseudotime of each cell: a numeric matrix with the row and column
  names of `weights`, such as
  [`slingshot::slingPseudotime()`](https://rdrr.io/pkg/slingshot/man/slingPseudotime.html);
  or, for a pseudotime that every lineage shares, as Monocle3 gives it,
  a numeric vector with one value per row of `weights`, in that order or
  named by cell.

- weights:

  A matrix or data frame of non-negative weights with one row per cell,
  named by cell, and one column per lineage, named by lineage and at
  least two of them, such as
  [`slingshot::slingCurveWeights()`](https://rdrr.io/pkg/slingshot/man/slingPseudotime.html),
  the cell weights of tradeSeq or the result of
  [`paths_from_graph()`](https://spriyansh.github.io/scMaSigPro/reference/paths_from_graph.md).

- min_weight:

  A single positive number: a cell is on a lineage when its weight there
  is at least `min_weight` and its pseudotime there is a finite number.
  (Default is 0.5)

- root_label:

  The path label of the cells on every lineage. (Default is "root")

- shared_sep:

  The separator of the lineage names in the label of the cells on some
  but not all lineages; pass it to
  [`sc.restruct()`](https://spriyansh.github.io/scMaSigPro/reference/sc.restruct.md)
  as `link_sep`. (Default is "*links*", the default of
  [`sc.restruct()`](https://spriyansh.github.io/scMaSigPro/reference/sc.restruct.md))

- verbose:

  Print a message when cells are left out. (Default is TRUE)

## Value

A data frame with one row per cell that is on at least one lineage, in
the order of the rows of `weights` and named by cell, and two columns:
`Pseudotime` (numeric) and `Path` (character). Pass it to
[`create_scmp()`](https://spriyansh.github.io/scMaSigPro/reference/create_scmp.md)
as `cell_data` with `ptime_col = "Pseudotime"` and `path_col = "Path"`,
or add its columns to the `colData()` of a `SingleCellExperiment` with
the same cells for
[`as_scmp()`](https://spriyansh.github.io/scMaSigPro/reference/as_scmp.md),
whose default column names they are.

## Details

Each cell is placed on the lineages where its weight is at least
`min_weight` and its pseudotime is known. A cell on one lineage gets the
name of that lineage as its path, a cell on every lineage gets
`root_label`, and a cell on two or more lineages but not all of them
gets the names of those lineages, in the order of the columns of
`weights`, joined by `shared_sep`. A cell on no lineage is left out. The
pseudotime of a cell is the mean of its pseudotimes on its lineages, or
its value when `pseudotime` is a vector.

The weights are compared with `min_weight` as they are given. Curve
weights of slingshot are close to 1 on every lineage for the cells of
the root; for weights that sum to 1 in each cell, as with
`slingCurveWeights(as.probs = TRUE)`, use a `min_weight` below 1 /
(number of lineages) or the root cells are left out. Soft weights can
place a few cells on a set of lineages that share no segment of the
trajectory beyond the root, such as two of three lineages that split at
the root; their label is built the same way, so relabel or remove them
before
[`sc.restruct()`](https://spriyansh.github.io/scMaSigPro/reference/sc.restruct.md)
if they are not a link.

## Input contract

scMaSigPro needs one pseudotime value and one path label per cell, and
nothing else from the tool that inferred the trajectory.
[`create_scmp()`](https://spriyansh.github.io/scMaSigPro/reference/create_scmp.md)
takes them as a data frame named by cell, such as the result of
`assign_paths()`, and
[`as_scmp()`](https://spriyansh.github.io/scMaSigPro/reference/as_scmp.md)
as two `colData()` columns of a `SingleCellExperiment`. Code outside
this package that reads a trajectory, such as a companion package for
Monocle3 objects or an interactive branch selector, returns that data
frame, or the R code that builds it with
[`paths_from_graph()`](https://spriyansh.github.io/scMaSigPro/reference/paths_from_graph.md)
and `assign_paths()`. Its path labels follow the rules in Details, so
that
[`sc.restruct()`](https://spriyansh.github.io/scMaSigPro/reference/sc.restruct.md)
can rebuild the paths that share a root.

## References

Street K, Risso D, Fletcher RB, Das D, Ngai J, Yosef N, Purdom E, Dudoit
S (2018). Slingshot: cell lineage and pseudotime inference for
single-cell transcriptomics. BMC Genomics 19, 477.
[doi:10.1186/s12864-018-4772-0](https://doi.org/10.1186/s12864-018-4772-0)

Van den Berge K, Roux de Bézieux H, Street K, Saelens W, Cannoodt R,
Saeys Y, Dudoit S, Clement L (2020). Trajectory-based differential
expression analysis for single-cell sequencing data. Nature
Communications 11, 1201.
[doi:10.1038/s41467-020-14766-3](https://doi.org/10.1038/s41467-020-14766-3)

## See also

[`paths_from_graph()`](https://spriyansh.github.io/scMaSigPro/reference/paths_from_graph.md)
for weights from a trajectory graph;
[`sc.restruct()`](https://spriyansh.github.io/scMaSigPro/reference/sc.restruct.md)
for the root and link paths.

Other input helpers:
[`as_scmp()`](https://spriyansh.github.io/scMaSigPro/reference/as_scmp.md),
[`create_scmp()`](https://spriyansh.github.io/scMaSigPro/reference/create_scmp.md),
[`paths_from_graph()`](https://spriyansh.github.io/scMaSigPro/reference/paths_from_graph.md)

## Author

Priyansh Srivastava <spriyansh29@gmail.com>

## Examples

``` r
# Three lineages: cells 1 and 2 are on all of them, cell 3 on A and B
weights <- matrix(
    c(
        1, 1, 1, 1, 0, 0,
        1, 1, 1, 0, 1, 0,
        1, 0.9, 0.2, 0, 0, 1
    ),
    ncol = 3,
    dimnames = list(paste0("cell", 1:6), c("A", "B", "C"))
)
pseudotime <- c(0.1, 0.5, 1.2, 2.1, 2.3, 1.8)
assign_paths(pseudotime, weights)
#>       Pseudotime      Path
#> cell1        0.1      root
#> cell2        0.5      root
#> cell3        1.2 A_links_B
#> cell4        2.1         A
#> cell5        2.3         B
#> cell6        1.8         C

# A pseudotime for each lineage, as slingshot gives it
pseudotime <- cbind(A = pseudotime, B = pseudotime + 0.1, C = pseudotime)
rownames(pseudotime) <- rownames(weights)
assign_paths(pseudotime, weights, shared_sep = "|")
#>       Pseudotime Path
#> cell1  0.1333333 root
#> cell2  0.5333333 root
#> cell3  1.2500000  A|B
#> cell4  2.1000000    A
#> cell5  2.4000000    B
#> cell6  1.8000000    C
```
