# Lineage weights from a trajectory graph

`paths_from_graph()` gives the lineage weights of the cells of a
trajectory graph, such as the principal graph of Monocle3 or a minimum
spanning tree of clusters, for
[`assign_paths()`](https://spriyansh.github.io/scMaSigPro/reference/assign_paths.md).
Each lineage is the shortest path from the root node to an end node, and
a cell is on it (weight 1) when its node is on that path.

## Usage

``` r
paths_from_graph(cell_node, edges, root, endpoints = NULL)
```

## Arguments

- cell_node:

  A vector with the node of each cell, named by cell. The nodes are
  compared with the node names of `edges` as character strings.

- edges:

  A matrix or data frame whose first two columns hold the two nodes of
  each edge of the graph, which is taken as undirected.

- root:

  The name of the root node.

- endpoints:

  The names of the end nodes, one lineage each; the default, `NULL`,
  takes every node with one edge other than the root, in the order in
  which they first appear in `edges`.

## Value

A numeric matrix with one row per cell, in the order and with the names
of `cell_node`, and one column per end node, named by it, holding 1 when
the node of the cell is on the path from the root to that end node and 0
otherwise.

## Details

The paths are found with
[`igraph::shortest_paths()`](https://r.igraph.org/reference/distances.html)
on the unweighted graph, so this function needs the suggested package
igraph. In a tree, as principal graphs and minimum spanning trees are,
the path from the root to each end node is the only one. Every node of
`cell_node` must be a node of `edges`, and every end node must be
reachable from the root.

## References

Van den Berge K, Roux de Bézieux H, Street K, Saelens W, Cannoodt R,
Saeys Y, Dudoit S, Clement L (2020). Trajectory-based differential
expression analysis for single-cell sequencing data. Nature
Communications 11, 1201.
[doi:10.1038/s41467-020-14766-3](https://doi.org/10.1038/s41467-020-14766-3)

Csárdi G, Nepusz T (2006). The igraph software package for complex
network research. InterJournal Complex Systems, 1695.
<https://igraph.org>

## See also

[`assign_paths()`](https://spriyansh.github.io/scMaSigPro/reference/assign_paths.md),
which turns these weights into path labels.

Other input helpers:
[`as_scmp()`](https://spriyansh.github.io/scMaSigPro/reference/as_scmp.md),
[`assign_paths()`](https://spriyansh.github.io/scMaSigPro/reference/assign_paths.md),
[`create_scmp()`](https://spriyansh.github.io/scMaSigPro/reference/create_scmp.md)

## Author

Priyansh Srivastava <spriyansh29@gmail.com>

## Examples

``` r
# A tree: the root R, a segment A-B shared by the end nodes C and D, and
# the end node E that branches off at A
edges <- rbind(
    c("R", "A"), c("A", "B"), c("B", "C"), c("B", "D"), c("A", "E")
)
cell_node <- c(
    cell1 = "R", cell2 = "A", cell3 = "B", cell4 = "C", cell5 = "D",
    cell6 = "E"
)
if (requireNamespace("igraph", quietly = TRUE)) {
    weights <- paths_from_graph(cell_node, edges, root = "R")
    print(weights)
    # With a pseudotime that every lineage shares
    assign_paths(c(0, 1, 2, 3, 3, 2), weights)
}
#>       C D E
#> cell1 1 1 1
#> cell2 1 1 1
#> cell3 1 1 0
#> cell4 1 0 0
#> cell5 0 1 0
#> cell6 0 0 1
#>       Pseudotime      Path
#> cell1          0      root
#> cell2          1      root
#> cell3          2 C_links_D
#> cell4          3         C
#> cell5          3         D
#> cell6          2         E
```
