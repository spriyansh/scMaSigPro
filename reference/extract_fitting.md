# Summaries of the selected model of a gene

`extract_fitting()` collects, for one gene, the p-value and R-squared of
its selected model, the coefficients, p-values and t statistics of its
terms and its influential observations;
[`sc.t.fit()`](https://spriyansh.github.io/scMaSigPro/reference/sc.t.fit.md)
calls it for each gene.

## Usage

``` r
extract_fitting(
  reg,
  lmf,
  model.glm.0,
  dis,
  family,
  name,
  vars.in,
  alfa,
  influ.info
)
```

## Arguments

- reg:

  The selected model, a `glm` fit.

- lmf:

  The full model.

- model.glm.0:

  The intercept-only model.

- dis:

  The design, a data frame with one column per term.

- family:

  The family of the fits.

- name:

  The name of the gene: the row name of `sol` and the column name of its
  entry in `influ.info`.

- vars.in:

  The names of the terms (the columns of `dis`); they fix the order of
  the coefficients, p-values and t statistics.

- alfa:

  The level at which a coefficient is significant.

- influ.info:

  The matrix of influential observations so far; a column for this gene
  is added when it has influential observations.

## Value

A list: the p-value (`p_value`) and R-squared (`bondad`) of the selected
model; the p-values (`p_valor`), coefficients (`coeff`) and t statistics
(`t`) of every term; the counts of the gene (`sig_profiles`); its row of
the results (`sol`, `NULL` when no coefficient is significant); the
updated `influ.info`; and the gene name (`feature_name`).

## See also

[`sc.t.fit()`](https://spriyansh.github.io/scMaSigPro/reference/sc.t.fit.md),
which calls it.
