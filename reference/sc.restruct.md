# Restructure the bins of paths that share a root

`sc.restruct()` rebuilds the bins so that every end path starts at the
root: the bins of each link path are added to the end paths it links,
the bins of the root to every end path, the binned pseudotime of the
later bins is shifted to follow them, and
[`pb_counts()`](https://spriyansh.github.io/scMaSigPro/reference/pb_counts.md)
recomputes the pseudo-bulk counts.

## Usage

``` r
sc.restruct(
  scmpObj,
  end_node_list,
  root_node,
  link_node_list,
  assay_name = "counts",
  aggregate = "sum",
  link_sep = "_links_",
  verbose = TRUE
)
```

## Arguments

- scmpObj:

  A
  [ScMaSigPro](https://spriyansh.github.io/scMaSigPro/reference/ScMaSigPro-class.md)
  object.

- end_node_list:

  A list of the labels of the end paths. The bins of a path that is not
  the root, an end path or a link path are left out, with a message when
  `verbose` is `TRUE`.

- root_node:

  The label of the root path. Each path is named once, in `root_node`,
  `end_node_list` or `link_node_list`.

- link_node_list:

  A list of the labels of the link paths, the segments that some end
  paths share; a label holds the labels of the paths it links, separated
  by `link_sep`.

- assay_name:

  The name of the assay of the `Sparse` slot that holds the counts of
  the cells.

- aggregate:

  How the counts of the cells of a bin are combined: `"sum"` or `"mean"`
  (rounded to whole numbers).

- link_sep:

  The separator in the labels of the link paths, matched as fixed text
  when the label contains it and as a regular expression otherwise, so
  `"|"` and `"\\|"` both work.

- verbose:

  Whether to print progress messages.

## Value

The object with the bins of every end path, from the root on, and their
pseudo-bulk counts in the `Dense` slot. The `Design`, `Profile`,
`Estimate` and `Significant` slots, computed from earlier bins, are
emptied.

## See also

[`assign_paths()`](https://spriyansh.github.io/scMaSigPro/reference/assign_paths.md),
which labels the root and link paths.

Other workflow steps:
[`sc.cluster.trend()`](https://spriyansh.github.io/scMaSigPro/reference/sc.cluster.trend.md),
[`sc.filter()`](https://spriyansh.github.io/scMaSigPro/reference/sc.filter.md),
[`sc.p.vector()`](https://spriyansh.github.io/scMaSigPro/reference/sc.p.vector.md),
[`sc.set.poly()`](https://spriyansh.github.io/scMaSigPro/reference/sc.set.poly.md),
[`sc.squeeze()`](https://spriyansh.github.io/scMaSigPro/reference/sc.squeeze.md),
[`sc.t.fit()`](https://spriyansh.github.io/scMaSigPro/reference/sc.t.fit.md)

## Author

Priyansh Srivastava <spriyansh29@gmail.com>

## Examples

``` r
data("multi.lin.sce", package = "scMaSigPro")
scmp <- as_scmp(multi.lin.sce,
    additional_params = list(
        labels_exist = TRUE, exist_ptime_col = "Monocle3_Pseudotime",
        exist_path_col = "group"
    ),
    verbose = FALSE
)
scmp <- sc.squeeze(scmp)
# Three end paths, one link path shared by two of them, and the root
scmp <- sc.restruct(scmp,
    end_node_list = list("path_Y_15", "path_Y_18", "path_Y_52"),
    root_node = "root", link_node_list = list("path_Y_18|path_Y_15"),
    link_sep = "|", verbose = FALSE
)
table(cDense(scmp)$Path)
#> 
#> path_Y_15 path_Y_18 path_Y_52 
#>        23        24        19 
```
