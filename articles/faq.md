# Frequently asked questions

Abstract

Short answers to common questions about the input, the settings and the
results of scMaSigPro, with links to the vignettes and help pages that
answer them in full.

The vignettes are on the package website,
<https://spriyansh.github.io/scMaSigPro/articles/>, and in R with
`browseVignettes("scMaSigPro")`; every function has a help page, such as
[`?sc.squeeze`](https://spriyansh.github.io/scMaSigPro/reference/sc.squeeze.md).

## Input

### What does scMaSigPro need from a trajectory?

One pseudotime value and one path label per cell, and the raw counts of
the cells.
[`as_scmp()`](https://spriyansh.github.io/scMaSigPro/reference/as_scmp.md)
takes them as a `SingleCellExperiment` with two `colData()` columns,
[`create_scmp()`](https://spriyansh.github.io/scMaSigPro/reference/create_scmp.md)
as a count matrix and a data frame. See the [Trajectory
input](https://spriyansh.github.io/scMaSigPro/articles/trajectory-input.html)
vignette and the “Input contract” section of
[`?assign_paths`](https://spriyansh.github.io/scMaSigPro/reference/assign_paths.md).

### How do I analyse a Monocle3 trajectory?

Take the principal graph, the nearest node of each cell, the root node
and the pseudotime from the `cell_data_set`, turn them into path labels
with
[`paths_from_graph()`](https://spriyansh.github.io/scMaSigPro/reference/paths_from_graph.md)
and
[`assign_paths()`](https://spriyansh.github.io/scMaSigPro/reference/assign_paths.md),
and pass the counts and the cell table to
[`create_scmp()`](https://spriyansh.github.io/scMaSigPro/reference/create_scmp.md).
The [Trajectory
input](https://spriyansh.github.io/scMaSigPro/articles/trajectory-input.html)
vignette has the code. Monocle3 itself is not a dependency, as it is not
on CRAN or Bioconductor.

### What about cells that belong to several paths?

Every cell gets one label. Cells on every path are the root, and cells
on some paths are a link, labelled with the names of those paths joined
by `_links_`;
[`assign_paths()`](https://spriyansh.github.io/scMaSigPro/reference/assign_paths.md)
makes these labels from lineage weights, such as those of slingshot.
Then either analyse each segment as a path of its own, or add the root
and the links to the paths that share them with
[`sc.restruct()`](https://spriyansh.github.io/scMaSigPro/reference/sc.restruct.md);
the [Shared
root](https://spriyansh.github.io/scMaSigPro/articles/shared-root.html)
vignette compares the two.

### Raw or normalised counts?

Raw counts. The model is negative binomial, a distribution of counts,
and the offset of each bin accounts for its sequencing depth. For values
that are not counts, such as log-normalised or scaled data, use
`family = gaussian()` with `offset = FALSE` in
[`sc.p.vector()`](https://spriyansh.github.io/scMaSigPro/reference/sc.p.vector.md).

### How many cells and bins are enough?

A polynomial of degree d has d + 1 coefficients per path, so each path
needs at least d + 1 bins, and a few more leave residual degrees of
freedom for the test.
[`plotBinTile()`](https://spriyansh.github.io/scMaSigPro/reference/plotBinTile.md)
shows the number of cells per bin; bins of very few cells give noisy
pseudo-bulk counts, which `prune_bins` removes and a lower `drop_fac`
avoids.

### The paths have different lengths. Does that matter?

Each path is binned along its own range of pseudotime, into a number of
bins that depends on its number of cells, and the model compares the
paths at the same binned pseudotime, the index of the bin. A path with
more bins extends beyond the others; `drop_trails = TRUE` in
[`sc.squeeze()`](https://spriyansh.github.io/scMaSigPro/reference/sc.squeeze.md)
cuts the longer paths to the binned pseudotime of the shortest, to
compare only the stretch they share.

## Settings

### Which bin rule should I use?

Sturges’ rule, the default, is the rule of the analyses of the article;
`drop_fac` below 1 gives fewer, larger bins, useful for data with many
zeros, and `split_bins` and `prune_bins` even out the number of cells
per bin. The [Model and
tuning](https://spriyansh.github.io/scMaSigPro/articles/model-and-tuning.html)
vignette describes every rule and compares them, and
[`?estBinSize`](https://spriyansh.github.io/scMaSigPro/reference/estBinSize.md)
gives their formulas.

### Which test should I use for the full model, chi-squared or F?

`test = "chisq"`, the default and the test of maSigPro, treats the
estimated dispersion as known; with few bins it calls more genes
significant than its level. `test = "F"` accounts for the estimate and
is more conservative. The [Model and
tuning](https://spriyansh.github.io/scMaSigPro/articles/model-and-tuning.html)
vignette shows both.

### When should I turn the offset off?

When the data are not raw counts, such as normalised or scaled values,
and with any link other than the log link, which the offset needs. With
raw counts keep it: it corrects for the different numbers of cells, and
so depths, of the bins.

### How do I fit the genes in parallel?

Pass a BiocParallel backend to
[`sc.p.vector()`](https://spriyansh.github.io/scMaSigPro/reference/sc.p.vector.md)
and
[`sc.t.fit()`](https://spriyansh.github.io/scMaSigPro/reference/sc.t.fit.md),
such as `BPPARAM = BiocParallel::MulticoreParam(4)` on Linux and macOS
or `BPPARAM = BiocParallel::SnowParam(4)` on any system, Windows
included. The results do not depend on the backend.

### Are the results reproducible?

The workflow has no random step except the k-means clustering of
[`sc.cluster.trend()`](https://spriyansh.github.io/scMaSigPro/reference/sc.cluster.trend.md);
call [`set.seed()`](https://rdrr.io/r/base/Random.html) before it for
the same clusters. Every setting a step used is kept in the object
([`showParams()`](https://spriyansh.github.io/scMaSigPro/reference/showParams.md)).

## Results

### No gene passes. What now?

Check the bins first: too few bins, or bins of very few cells, leave the
test little to work with
([`plotBinTile()`](https://spriyansh.github.io/scMaSigPro/reference/plotBinTile.md)).
Then the thresholds: `p_value` of
[`sc.p.vector()`](https://spriyansh.github.io/scMaSigPro/reference/sc.p.vector.md)
is, with the default correction, a false discovery rate, and `rsq` of
[`sc.filter()`](https://spriyansh.github.io/scMaSigPro/reference/sc.filter.md)
a threshold on the fit;
[`scmpResults()`](https://spriyansh.github.io/scMaSigPro/reference/scmpResults.md)
reports both values for every gene. On raw counts, turning the offset
off can also hide most genes, as the [Model and
tuning](https://spriyansh.github.io/scMaSigPro/articles/model-and-tuning.html)
vignette shows.

### Why do my results differ from those of version 0.0.4 or the 2024 article?

Version 0.6.0 changed how the bins are made and how the offset is
computed, honours the `link` argument and chooses the reference path in
the same way in every locale; each change is listed under “Changes
affecting results” in the
[news](https://spriyansh.github.io/scMaSigPro/news/index.html). To
repeat an earlier analysis exactly, install the version that made it,
for example `BiocManager::install("BioBam/scMaSigPro@v0.0.4")`.

### I saved an object with an earlier version. Can I still use it?

Yes: run `updateObject()` on it after
[`readRDS()`](https://rdrr.io/r/base/readRDS.html) or
[`load()`](https://rdrr.io/r/base/load.html). The steps stop with
“update it with updateObject() first” until then. See
[`help("updateObject,ScMaSigPro-method")`](https://spriyansh.github.io/scMaSigPro/reference/updateObject-ScMaSigPro-method.md)
and the [maSigPro and the ScMaSigPro
class](https://spriyansh.github.io/scMaSigPro/articles/masigpro-and-class.html)
vignette.

### How do I get the results into my SingleCellExperiment?

`scmpToRowData(sce, scmp_ob)` writes the columns of
[`scmpResults()`](https://spriyansh.github.io/scMaSigPro/reference/scmpResults.md),
with the prefix `scmp_`, into the `rowData()` of `sce`, matching the
genes by name.

### How long does an analysis take?

The fits run gene by gene, so the time grows with the number of genes
and with the number of bins; the number of cells matters only for the
binning. On the 100 genes and 16 bins of `splat.sim` the whole workflow
takes well under a second. A parallel backend fits several genes at a
time.

## Citing scMaSigPro

``` r

citation("scMaSigPro")
#> To cite scMaSigPro in publications use:
#> 
#>   Srivastava P, Benegas Coll M, Götz S, Nueda MJ, Conesa A (2024).
#>   "scMaSigPro: differential expression analysis along single-cell
#>   trajectories." _Bioinformatics_, *40*(7), btae443.
#>   doi:10.1093/bioinformatics/btae443
#>   <https://doi.org/10.1093/bioinformatics/btae443>.
#> 
#> A BibTeX entry for LaTeX users is
#> 
#>   @Article{,
#>     title = {{scMaSigPro}: differential expression analysis along single-cell trajectories},
#>     author = {Priyansh Srivastava and Marta {Benegas Coll} and Stefan Götz and María José Nueda and Ana Conesa},
#>     journal = {Bioinformatics},
#>     year = {2024},
#>     volume = {40},
#>     number = {7},
#>     pages = {btae443},
#>     doi = {10.1093/bioinformatics/btae443},
#>   }
```

## Session information

``` r

sessionInfo()
#> R version 4.6.1 (2026-06-24)
#> Platform: x86_64-pc-linux-gnu
#> Running under: Ubuntu 24.04.5 LTS
#> 
#> Matrix products: default
#> BLAS:   /usr/lib/x86_64-linux-gnu/openblas-pthread/libblas.so.3 
#> LAPACK: /usr/lib/x86_64-linux-gnu/openblas-pthread/libopenblasp-r0.3.26.so;  LAPACK version 3.12.0
#> 
#> locale:
#>  [1] LC_CTYPE=C.UTF-8       LC_NUMERIC=C           LC_TIME=C.UTF-8       
#>  [4] LC_COLLATE=C.UTF-8     LC_MONETARY=C.UTF-8    LC_MESSAGES=C.UTF-8   
#>  [7] LC_PAPER=C.UTF-8       LC_NAME=C              LC_ADDRESS=C          
#> [10] LC_TELEPHONE=C         LC_MEASUREMENT=C.UTF-8 LC_IDENTIFICATION=C   
#> 
#> time zone: UTC
#> tzcode source: system (glibc)
#> 
#> attached base packages:
#> [1] stats     graphics  grDevices utils     datasets  methods   base     
#> 
#> other attached packages:
#> [1] BiocStyle_2.40.0
#> 
#> loaded via a namespace (and not attached):
#>  [1] digest_0.6.39       desc_1.4.3          R6_2.6.1           
#>  [4] bookdown_0.48       fastmap_1.2.0       xfun_0.62          
#>  [7] cachem_1.1.0        knitr_1.52          htmltools_0.5.9    
#> [10] rmarkdown_2.32      lifecycle_1.0.5     cli_3.6.6          
#> [13] sass_0.4.10         pkgdown_2.2.1       textshaping_1.0.5  
#> [16] jquerylib_0.1.4     systemfonts_1.3.2   compiler_4.6.1     
#> [19] tools_4.6.1         ragg_1.5.2          bslib_0.12.0       
#> [22] evaluate_1.0.5      yaml_2.3.12         BiocManager_1.30.27
#> [25] otel_0.2.0          jsonlite_2.0.0      rlang_1.3.0        
#> [28] fs_2.1.0
```
