# Pseudo-bulk counts of the Dense slot

`eDense()` returns an assay of the `Dense` slot of a
[ScMaSigPro](https://spriyansh.github.io/scMaSigPro/reference/ScMaSigPro-class.md)
object, genes by bins, as a dense matrix; `eDense<-` sets one.

## Usage

``` r
eDense(object, i = "bulk.counts", ...)

eDense(object, i = "bulk.counts") <- value

# S4 method for class 'ScMaSigPro'
eDense(object, i = "bulk.counts", ...)

# S4 method for class 'ScMaSigPro,character'
eDense(object, i = "bulk.counts") <- value

# S4 method for class 'ScMaSigPro,missing'
eDense(object, i = "bulk.counts") <- value
```

## Arguments

- object:

  A
  [ScMaSigPro](https://spriyansh.github.io/scMaSigPro/reference/ScMaSigPro-class.md)
  object.

- i:

  The name of the assay. With the default, `"bulk.counts"`, `eDense()`
  returns the first assay when no assay has that name.

- ...:

  Not used.

- value:

  A matrix, a `dgCMatrix` or a data frame of numbers, with one row per
  gene and one column per bin of the `Dense` slot. Its row and column
  names, when present, must be the gene and bin names of the slot in the
  same order; missing names are filled in.

## Value

`eDense()`: a numeric matrix. `eDense<-`: the updated object.

## Details

`eDense<-` stores `value` as a `dgCMatrix`. It gives a message when it
creates a new assay and a warning when it replaces one.

## See also

Other accessors:
[`cDense()`](https://spriyansh.github.io/scMaSigPro/reference/cDense.md),
[`cSparse()`](https://spriyansh.github.io/scMaSigPro/reference/cSparse.md),
[`eSparse()`](https://spriyansh.github.io/scMaSigPro/reference/eSparse.md),
[`pathAssign()`](https://spriyansh.github.io/scMaSigPro/reference/pathAssign.md),
[`predictors()`](https://spriyansh.github.io/scMaSigPro/reference/predictors.md)

## Examples

``` r
data("scmp.ob", package = "scMaSigPro")
bulk <- eDense(scmp.ob)
bulk[1:3, 1:4]
#>       Path1_bin_1 Path1_bin_2 Path1_bin_3 Path1_bin_4
#> Gene1          16          24          11          31
#> Gene2        6335        7337        3375        9161
#> Gene3         201         245         105         216

# Add a second assay
eDense(scmp.ob, "logcounts") <- log1p(bulk)
#> Creating a new assay as 'logcounts'.
```
