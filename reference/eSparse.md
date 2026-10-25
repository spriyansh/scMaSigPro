# Cell counts of the Sparse slot

`eSparse()` returns an assay of the `Sparse` slot of a
[ScMaSigPro](https://spriyansh.github.io/scMaSigPro/reference/ScMaSigPro-class.md)
object, genes by cells, as a dense matrix; `eSparse<-` sets one.

## Usage

``` r
eSparse(object, i = "counts", ...)

eSparse(object, i = "counts") <- value

# S4 method for class 'ScMaSigPro'
eSparse(object, i = "counts", ...)

# S4 method for class 'ScMaSigPro,character'
eSparse(object, i = "counts") <- value

# S4 method for class 'ScMaSigPro,missing'
eSparse(object, i = "counts") <- value
```

## Arguments

- object:

  A
  [ScMaSigPro](https://spriyansh.github.io/scMaSigPro/reference/ScMaSigPro-class.md)
  object.

- i:

  The name of the assay. With the default, `"counts"`, `eSparse()`
  returns the first assay when no assay has that name.

- ...:

  Not used.

- value:

  A matrix, a `dgCMatrix` or a data frame of numbers, with one row per
  gene and one column per cell of the `Sparse` slot. Its row and column
  names, when present, must be the gene and cell names of the slot in
  the same order; missing names are filled in.

## Value

`eSparse()`: a numeric matrix. `eSparse<-`: the updated object.

## Details

`eSparse<-` stores `value` as a `dgCMatrix`. It gives a message when it
creates a new assay and a warning when it replaces one.

## See also

Other accessors:
[`cDense()`](https://spriyansh.github.io/scMaSigPro/reference/cDense.md),
[`cSparse()`](https://spriyansh.github.io/scMaSigPro/reference/cSparse.md),
[`eDense()`](https://spriyansh.github.io/scMaSigPro/reference/eDense.md),
[`pathAssign()`](https://spriyansh.github.io/scMaSigPro/reference/pathAssign.md),
[`predictors()`](https://spriyansh.github.io/scMaSigPro/reference/predictors.md)

## Examples

``` r
data("scmp.ob", package = "scMaSigPro")
counts <- eSparse(scmp.ob)
counts[1:3, 1:4]
#>       Cell1 Cell2 Cell3 Cell4
#> Gene1     0     2     1     2
#> Gene2   436   371   432   427
#> Gene3    35    13    17    18

# Add a second assay
eSparse(scmp.ob, "logcounts") <- log1p(counts)
#> Creating a new assay as 'logcounts'.
```
