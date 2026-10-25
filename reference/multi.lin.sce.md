# Simulated multifurcating trajectory

A `SingleCellExperiment` with the counts of 501 genes in 1977 cells of a
simulated trajectory with three end points and a shared root, with the
pseudotime and the path labels of each cell. The vignettes [Trajectory
input](https://spriyansh.github.io/scMaSigPro/articles/trajectory-input.html)
and [Shared
root](https://spriyansh.github.io/scMaSigPro/articles/shared-root.html)
analyse it.

## Usage

``` r
data("multi.lin.sce", package = "scMaSigPro")
```

## Format

A `SingleCellExperiment` with 501 genes (rows) and 1977 cells (columns):

- `counts` assay:

  The simulated counts.

- `colData`:

  `cell_id`, the cell name; `group`, the path label: `"root"` for the
  cells on all three lineages, `"path_Y_15"`, `"path_Y_18"` or
  `"path_Y_52"` for the cells on one, and `"path_Y_18|path_Y_15"` for
  the cells on these two; `Monocle3_Pseudotime`, the pseudotime of
  Monocle 3.

- `reducedDims`:

  `TSNE`, the two dimensions of the t-SNE embedding.

The `rowData` has no columns.

## Source

The multifurcating simulation of Van den Berge et al. (2020), made with
dyntoy (Saelens et al. 2019): `multifurcating_4.rds` of
<https://github.com/statOmics/tradeSeqPaper>. Monocle 3 (Cao et al.
2019) normalised the counts, embedded the cells with PCA and t-SNE,
learnt the principal graph and gave the pseudotime from the root node
`Y_51`. Each cell is on the lineages whose shortest path from the root,
on that graph, holds its nearest node, as in the Monocle 3 steps of the
tradeSeq vignette; three lineages are kept and the cells on none of them
are left out. The script is
`system.file("scripts", "make-multi-lin-sce.R", package = "scMaSigPro")`.

## References

Van den Berge K, Roux de Bézieux H, Street K, Saelens W, Cannoodt R,
Saeys Y, Dudoit S, Clement L (2020). Trajectory-based differential
expression analysis for single-cell sequencing data. Nature
Communications 11, 1201.
[doi:10.1038/s41467-020-14766-3](https://doi.org/10.1038/s41467-020-14766-3)

Saelens W, Cannoodt R, Todorov H, Saeys Y (2019). A comparison of
single-cell trajectory inference methods. Nature Biotechnology 37(5),
547-554.
[doi:10.1038/s41587-019-0071-9](https://doi.org/10.1038/s41587-019-0071-9)

Cao J, Spielmann M, Qiu X, Huang X, Ibrahim DM, Hill AJ, Zhang F,
Mundlos S, Christiansen L, Steemers FJ, Trapnell C, Shendure J (2019).
The single-cell transcriptional landscape of mammalian organogenesis.
Nature 566(7745), 496-502.
[doi:10.1038/s41586-019-0969-x](https://doi.org/10.1038/s41586-019-0969-x)

## See also

Other datasets:
[`scmp.ob`](https://spriyansh.github.io/scMaSigPro/reference/scmp.ob.md),
[`splat.sim`](https://spriyansh.github.io/scMaSigPro/reference/splat.sim.md)

## Author

Priyansh Srivastava <spriyansh29@gmail.com>

## Examples

``` r
data("multi.lin.sce", package = "scMaSigPro")
table(SummarizedExperiment::colData(multi.lin.sce)$group)
#> 
#>           path_Y_15           path_Y_18 path_Y_18|path_Y_15           path_Y_52 
#>                 307                 676                  43                 498 
#>                root 
#>                 453 
```
