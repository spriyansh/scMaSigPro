# Gene lists as a 0/1 table

`fromListWithNames()` turns a named list of gene lists into a data frame
with one row per gene, named by gene, and a 0/1 column per list, as
[`UpSetR::fromList()`](https://rdrr.io/pkg/UpSetR/man/fromList.html)
does, with the row names.

## Usage

``` r
fromListWithNames(input)
```

## Arguments

- input:

  A named list of character vectors.

## Value

A data frame of 0 and 1, one row per gene and one column per list.

## See also

[`plotIntersect()`](https://spriyansh.github.io/scMaSigPro/reference/plotIntersect.md),
which calls it.
