# ParameterConfig

The settings of an analysis: the names of the columns of the cell and
bin metadata and the parameters of every step of the workflow.

## Value

An object of class `ParameterConfig`: the settings of an analysis, one
value per slot. Every step of the workflow records the settings it used
in the `Parameters` slot of a
[ScMaSigPro](https://spriyansh.github.io/scMaSigPro/reference/ScMaSigPro-class.md)
object, and
[`showParams()`](https://spriyansh.github.io/scMaSigPro/reference/showParams.md)
lists them.

## Details

`validObject()` checks that every slot except `distribution` holds a
single non-missing value, that `min_counts` and `min_bins_expressed` are
at least 0, that `test` is `"chisq"` or `"F"` and that `distribution` is
a [`stats::family()`](https://rdrr.io/r/stats/family.html) object.

## Slots

- `bin_ptime_col`:

  The column of the bin metadata with the binned pseudotime.

- `path_prefix`:

  The default path column of
  [`as_scmp()`](https://spriyansh.github.io/scMaSigPro/reference/as_scmp.md).

- `root_label`:

  The label of the root cells given to
  [`as_scmp()`](https://spriyansh.github.io/scMaSigPro/reference/as_scmp.md),
  kept for reference.

- `ptime_col`:

  The column of the cell metadata with the pseudotime.

- `bin_method`:

  The rule for the number of bins of
  [`sc.squeeze()`](https://spriyansh.github.io/scMaSigPro/reference/sc.squeeze.md).

- `path_col`:

  The column of the cell and bin metadata with the path labels.

- `bin_col`:

  The column of the bin metadata with the bin names.

- `bin_size_col`:

  The column of the bin metadata with the number of cells of each bin.

- `bin_mem_col`:

  The column of the bin metadata with the cells of each bin.

- `anno_col`:

  The column of the cell metadata with cell annotations; not used by the
  steps.

- `g`:

  The number of genes that
  [`sc.p.vector()`](https://spriyansh.github.io/scMaSigPro/reference/sc.p.vector.md)
  fitted: those left after removing the genes with fewer than `min_na`
  values, with zero counts in every bin, or below `min_counts` or
  `min_bins_expressed`.

- `p_value`:

  The level of
  [`sc.p.vector()`](https://spriyansh.github.io/scMaSigPro/reference/sc.p.vector.md)
  and
  [`sc.t.fit()`](https://spriyansh.github.io/scMaSigPro/reference/sc.t.fit.md).

- `min_na`:

  The minimum number of bins with a value (not `NA`) that
  [`sc.p.vector()`](https://spriyansh.github.io/scMaSigPro/reference/sc.p.vector.md)
  required; it only removes genes from counts with missing values.

- `mt_correction`:

  The multiple-testing correction of
  [`sc.p.vector()`](https://spriyansh.github.io/scMaSigPro/reference/sc.p.vector.md).

- `link`:

  The link function of the fits.

- `epsilon`:

  The convergence tolerance of the fits.

- `selection_method`:

  The stepwise method of
  [`sc.t.fit()`](https://spriyansh.github.io/scMaSigPro/reference/sc.t.fit.md).

- `offset`:

  Whether the fits used the offset of each bin, the logarithm of its
  size factor (see
  [`sc.p.vector()`](https://spriyansh.github.io/scMaSigPro/reference/sc.p.vector.md)).

- `max_it`:

  The maximum number of iterations of the fits.

- `poly_degree`:

  The degree of the polynomial of
  [`sc.set.poly()`](https://spriyansh.github.io/scMaSigPro/reference/sc.set.poly.md).

- `distribution`:

  The family of the fits, a
  [`stats::family()`](https://rdrr.io/r/stats/family.html) object; with
  an estimated theta it also holds `theta_estimate` and `theta_gene`
  (see
  [`sc.p.vector()`](https://spriyansh.github.io/scMaSigPro/reference/sc.p.vector.md)).

- `cluster_method`:

  The clustering method of
  [`sc.cluster.trend()`](https://spriyansh.github.io/scMaSigPro/reference/sc.cluster.trend.md).

- `use_dim`:

  How
  [`sc.cluster.trend()`](https://spriyansh.github.io/scMaSigPro/reference/sc.cluster.trend.md)
  filled missing values: by column or by row.

- `fill_na`:

  What
  [`sc.cluster.trend()`](https://spriyansh.github.io/scMaSigPro/reference/sc.cluster.trend.md)
  filled missing values with.

- `min_counts`:

  The minimum total count over the bins that
  [`sc.p.vector()`](https://spriyansh.github.io/scMaSigPro/reference/sc.p.vector.md)
  required of a gene (0: no minimum).

- `min_bins_expressed`:

  The minimum number of bins with a count above 0 that
  [`sc.p.vector()`](https://spriyansh.github.io/scMaSigPro/reference/sc.p.vector.md)
  required of a gene (0: no minimum).

- `test`:

  The test of the full model of each gene that
  [`sc.p.vector()`](https://spriyansh.github.io/scMaSigPro/reference/sc.p.vector.md)
  was asked for: `"chisq"`, the chi-squared test, or `"F"`, the F test.

## See also

Other classes:
[`Estimates`](https://spriyansh.github.io/scMaSigPro/reference/Estimates-class.md),
[`MatrixDesign`](https://spriyansh.github.io/scMaSigPro/reference/MatrixDesign-class.md),
[`ScMaSigPro`](https://spriyansh.github.io/scMaSigPro/reference/ScMaSigPro-class.md),
[`Significant`](https://spriyansh.github.io/scMaSigPro/reference/Significant-class.md),
[`VariableProfiles`](https://spriyansh.github.io/scMaSigPro/reference/VariableProfiles-class.md),
[`updateObject,ScMaSigPro-method`](https://spriyansh.github.io/scMaSigPro/reference/updateObject-ScMaSigPro-method.md)

## Examples

``` r
data("scmp.ob", package = "scMaSigPro")
showParams(scmp.ob)
#>            parameters                  value
#> 1       bin_ptime_col scmp_binned_pseudotime
#> 2         path_prefix                   Path
#> 3          root_label                   root
#> 4           ptime_col             Pseudotime
#> 5    selection_method               backward
#> 6          bin_method                Sturges
#> 7            path_col                   Path
#> 8             bin_col               scmp_bin
#> 9        bin_size_col          scmp_bin_size
#> 10        bin_mem_col       scmp_bin_members
#> 11           anno_col              cell_type
#> 12                  g                    100
#> 13            p_value                   0.05
#> 14             min_na                      6
#> 15      mt_correction                     BH
#> 16               link                    log
#> 17            epsilon                  1e-08
#> 18             offset                   TRUE
#> 19             max_it                    100
#> 20        poly_degree                      1
#> 21     cluster_method                 hclust
#> 22            use_dim                    col
#> 23            fill_na                   zero
#> 24         min_counts                      0
#> 25 min_bins_expressed                      0
#> 26               test                  chisq
#> 27       distribution  Negative Binomial(10)
```
