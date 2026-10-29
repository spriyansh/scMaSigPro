# scMaSigPro: Differential Expression Analysis Along Single-Cell Trajectories

scMaSigPro finds genes whose expression changes along pseudotime and
differs between the branching paths of a single-cell trajectory. The
cells of each path are pseudo-bulked into pseudotime bins, and the
polynomial generalised linear model of maSigPro is fitted to the binned
counts, by default with a negative binomial family and size-factor
offsets (Srivastava et al. 2024).

## Workflow

1.  [`as_scmp()`](https://spriyansh.github.io/scMaSigPro/reference/as_scmp.md)
    or
    [`create_scmp()`](https://spriyansh.github.io/scMaSigPro/reference/create_scmp.md):
    a
    [ScMaSigPro](https://spriyansh.github.io/scMaSigPro/reference/ScMaSigPro-class.md)
    object from the counts and one pseudotime value and one path label
    per cell.

2.  [`sc.squeeze()`](https://spriyansh.github.io/scMaSigPro/reference/sc.squeeze.md):
    pseudotime bins and pseudo-bulk counts for each path;
    [`sc.restruct()`](https://spriyansh.github.io/scMaSigPro/reference/sc.restruct.md)
    adds the bins of a shared root to every path.

3.  [`sc.set.poly()`](https://spriyansh.github.io/scMaSigPro/reference/sc.set.poly.md):
    the polynomial design.

4.  [`sc.p.vector()`](https://spriyansh.github.io/scMaSigPro/reference/sc.p.vector.md):
    the test of each gene's full model, which selects the genes with a
    non-flat profile.

5.  [`sc.t.fit()`](https://spriyansh.github.io/scMaSigPro/reference/sc.t.fit.md):
    stepwise selection of the significant terms of each of these genes.

6.  [`sc.filter()`](https://spriyansh.github.io/scMaSigPro/reference/sc.filter.md):
    lists of significant genes;
    [`sc.cluster.trend()`](https://spriyansh.github.io/scMaSigPro/reference/sc.cluster.trend.md)
    clusters them.

7.  [`plotTrend()`](https://spriyansh.github.io/scMaSigPro/reference/plotTrend.md),
    [`plotTrendCluster()`](https://spriyansh.github.io/scMaSigPro/reference/plotTrendCluster.md),
    [`plotIntersect()`](https://spriyansh.github.io/scMaSigPro/reference/plotIntersect.md):
    plots;
    [`scmpResults()`](https://spriyansh.github.io/scMaSigPro/reference/scmpResults.md):
    the results as a table with one row per gene.

A step run again empties the slots of the later steps, which were
computed from what it replaces; the `Parameters` slot keeps the settings
of every step.

## Input

The workflow needs one pseudotime value and one path label per cell.
[`assign_paths()`](https://spriyansh.github.io/scMaSigPro/reference/assign_paths.md)
and
[`paths_from_graph()`](https://spriyansh.github.io/scMaSigPro/reference/paths_from_graph.md)
build them from the lineage weights or the graph of a trajectory tool;
the "Input contract" section of
[`assign_paths()`](https://spriyansh.github.io/scMaSigPro/reference/assign_paths.md)
describes the format.

## Documentation

`browseVignettes("scMaSigPro")` lists the vignettes. They and these help
pages are also on the package website,
<https://spriyansh.github.io/scMaSigPro/>:

- Quick start:
  <https://spriyansh.github.io/scMaSigPro/articles/scMaSigPro.html>

- Trajectory input:
  <https://spriyansh.github.io/scMaSigPro/articles/trajectory-input.html>

- Shared root:
  <https://spriyansh.github.io/scMaSigPro/articles/shared-root.html>

- Model and tuning:
  <https://spriyansh.github.io/scMaSigPro/articles/model-and-tuning.html>

- maSigPro and the ScMaSigPro class:
  <https://spriyansh.github.io/scMaSigPro/articles/masigpro-and-class.html>

- Frequently asked questions:
  <https://spriyansh.github.io/scMaSigPro/articles/faq.html>

## References

Srivastava P, Benegas Coll M, Götz S, Nueda MJ, Conesa A (2024).
scMaSigPro: differential expression analysis along single-cell
trajectories. Bioinformatics 40(7), btae443.
[doi:10.1093/bioinformatics/btae443](https://doi.org/10.1093/bioinformatics/btae443)

Conesa A, Nueda MJ, Ferrer A, Talón M (2006). maSigPro: a method to
identify significantly differential expression profiles in time-course
microarray experiments. Bioinformatics 22(9), 1096-1102.
[doi:10.1093/bioinformatics/btl056](https://doi.org/10.1093/bioinformatics/btl056)

Nueda MJ, Tarazona S, Conesa A (2014). Next maSigPro: updating maSigPro
bioconductor package for RNA-seq time series. Bioinformatics 30(18),
2598-2602.
[doi:10.1093/bioinformatics/btu333](https://doi.org/10.1093/bioinformatics/btu333)

## See also

Useful links:

- <https://github.com/BioBam/scMaSigPro/>

- <https://spriyansh.github.io/scMaSigPro/>

- Report bugs at <https://github.com/BioBam/scMaSigPro/issues>

## Author

**Maintainer**: Priyansh Srivastava <spriyansh29@gmail.com>
([ORCID](https://orcid.org/0000-0003-2618-5296))

Authors:

- Priyansh Srivastava <spriyansh29@gmail.com>
  ([ORCID](https://orcid.org/0000-0003-2618-5296))

- Ana Conesa <ana.conesa@csic.es>
  ([ORCID](https://orcid.org/0000-0001-9597-311X))

- María José Nueda <mj.nueda@ua.es>
  ([ORCID](https://orcid.org/0000-0002-1666-4771))

- Marta Benegas Coll <martabc22@gmail.com>
  ([ORCID](https://orcid.org/0009-0004-9886-2828))

Other contributors:

- European Union (Horizon 2020 Marie Skłodowska-Curie Grant Agreement
  No 953407) \[funder\]
