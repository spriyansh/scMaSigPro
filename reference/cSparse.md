# Cell metadata of the Sparse slot

`cSparse()` returns the cell metadata (`colData`) of the `Sparse` slot
of a
[ScMaSigPro](https://spriyansh.github.io/scMaSigPro/reference/ScMaSigPro-class.md)
object; `cSparse<-` replaces it.

## Usage

``` r
cSparse(object, value = "missing")

cSparse(object) <- value

# S4 method for class 'ScMaSigPro'
cSparse(object, value = "missing")

# S4 method for class 'ScMaSigPro'
cSparse(object) <- value
```

## Arguments

- object:

  A
  [ScMaSigPro](https://spriyansh.github.io/scMaSigPro/reference/ScMaSigPro-class.md)
  object.

- value:

  A data frame with one row per cell, named after the cells of the
  `Sparse` slot and in their order, as `cSparse()` returns it. Without
  row names, the rows are taken in the order of the cells.

## Value

`cSparse(object)`: a data frame with one row per cell. `cSparse<-` and
`cSparse(object, value)`: the updated object, returned invisibly by the
latter.

## Details

The cells are the columns of the counts, and
[`sc.squeeze()`](https://spriyansh.github.io/scMaSigPro/reference/sc.squeeze.md)
finds them by name, so a `value` with other cells or another order is an
error. `cSparse(object, value)` sets `value` as
`cSparse(object) <- value` does.

## See also

Other accessors:
[`cDense()`](https://spriyansh.github.io/scMaSigPro/reference/cDense.md),
[`eDense()`](https://spriyansh.github.io/scMaSigPro/reference/eDense.md),
[`eSparse()`](https://spriyansh.github.io/scMaSigPro/reference/eSparse.md),
[`pathAssign()`](https://spriyansh.github.io/scMaSigPro/reference/pathAssign.md),
[`predictors()`](https://spriyansh.github.io/scMaSigPro/reference/predictors.md)

## Examples

``` r
data("scmp.ob", package = "scMaSigPro")
cells <- cSparse(scmp.ob)
head(cells[, c("Pseudotime", "Path")])
#>       Pseudotime  Path
#> Cell1         42 Path2
#> Cell2         41 Path1
#> Cell3         23 Path1
#> Cell4         42 Path1
#> Cell5         79 Path2
#> Cell6          1 Path1

# Add a column to the cell metadata
cells$early <- cells$Pseudotime < 50
cSparse(scmp.ob) <- cells
```
