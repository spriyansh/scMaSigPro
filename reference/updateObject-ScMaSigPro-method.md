# Update an ScMaSigPro object to the current class definition

`updateObject()` brings a
[ScMaSigPro](https://spriyansh.github.io/scMaSigPro/reference/ScMaSigPro-class.md)
object saved by an earlier version of scMaSigPro, for example with
[`saveRDS()`](https://rdrr.io/r/base/readRDS.html) or
[`save()`](https://rdrr.io/r/base/save.html), up to date. Call it on
such an object after [`readRDS()`](https://rdrr.io/r/base/readRDS.html)
or [`load()`](https://rdrr.io/r/base/load.html), before the next step of
the workflow.

## Usage

``` r
# S4 method for class 'ScMaSigPro'
updateObject(object, ..., verbose = FALSE)
```

## Arguments

- object:

  A
  [ScMaSigPro](https://spriyansh.github.io/scMaSigPro/reference/ScMaSigPro-class.md)
  object.

- ...:

  Passed to the `updateObject()` methods of the `Sparse` and `Dense`
  slots.

- verbose:

  Whether to print a message when the object is updated.

## Value

The updated `ScMaSigPro` object.

## Details

Objects saved before the `version` slot was added (scMaSigPro 0.0.5 and
earlier) fail `validObject()`; `updateObject()` adds the slot with the
version of the package that is running. For every object, the `Sparse`
and `Dense` slots are passed to the `updateObject()` method of
SingleCellExperiment, and the result is checked with `validObject()`. A
current object, such as one created by this version of the package, is
returned unchanged.

Objects saved before the `min_counts` and `min_bins_expressed`
parameters existed get both set to 0, the values that keep every gene
([`sc.p.vector()`](https://spriyansh.github.io/scMaSigPro/reference/sc.p.vector.md)
had no such filter then), and the version of the package that is
running.

Objects saved before the `test` parameter existed get it set to
`"chisq"`, the test of
[`sc.p.vector()`](https://spriyansh.github.io/scMaSigPro/reference/sc.p.vector.md)
then, and the version of the package that is running.

Objects saved with the `log_offset` parameter lose it. The fits of an
object saved with `log_offset = FALSE`, the default, used the size
factor of each bin as the offset: `updateObject()` stores the logarithm
of the size factors instead, the offset of
[`sc.p.vector()`](https://spriyansh.github.io/scMaSigPro/reference/sc.p.vector.md)
and
[`sc.t.fit()`](https://spriyansh.github.io/scMaSigPro/reference/sc.t.fit.md),
and warns that the fits in the `Profile` and `Estimate` slots were made
with the size factors, until
[`sc.p.vector()`](https://spriyansh.github.io/scMaSigPro/reference/sc.p.vector.md)
is run again. The offsets of an object saved with `log_offset = TRUE`
are kept. An object whose fits used no offset, or that was not fitted,
keeps no offset: the offsets that an earlier fit left in it, or the
zeros of
[`sc.set.poly()`](https://spriyansh.github.io/scMaSigPro/reference/sc.set.poly.md),
are removed.

Objects fitted with `link = "identity"` by scMaSigPro 0.0.5 or earlier
hold a family labelled with the identity link but with the link
functions of the log link, with which their fits were made:
`updateObject()` warns, and
[`sc.t.fit()`](https://spriyansh.github.io/scMaSigPro/reference/sc.t.fit.md)
stops on them, until
[`sc.p.vector()`](https://spriyansh.github.io/scMaSigPro/reference/sc.p.vector.md)
is run again.

Objects binned by scMaSigPro 0.0.4 cannot be updated: their `Dense` slot
is not a valid `SingleCellExperiment`, and the cell metadata of their
`Sparse` slot may not be in the order of the counts. `updateObject()`
stops with the command that bins the cells of the `Sparse` slot again
with
[`sc.squeeze()`](https://spriyansh.github.io/scMaSigPro/reference/sc.squeeze.md);
the later steps of the workflow must then be run again.

## See also

[ScMaSigPro](https://spriyansh.github.io/scMaSigPro/reference/ScMaSigPro-class.md)

Other classes:
[`Estimates`](https://spriyansh.github.io/scMaSigPro/reference/Estimates-class.md),
[`MatrixDesign`](https://spriyansh.github.io/scMaSigPro/reference/MatrixDesign-class.md),
[`ParameterConfig`](https://spriyansh.github.io/scMaSigPro/reference/ParameterConfig-class.md),
[`ScMaSigPro`](https://spriyansh.github.io/scMaSigPro/reference/ScMaSigPro-class.md),
[`Significant`](https://spriyansh.github.io/scMaSigPro/reference/Significant-class.md),
[`VariableProfiles`](https://spriyansh.github.io/scMaSigPro/reference/VariableProfiles-class.md)

## Author

Priyansh Srivastava <spriyansh29@gmail.com>

## Examples

``` r
data("scmp.ob", package = "scMaSigPro")
path <- tempfile(fileext = ".rds")
saveRDS(scmp.ob, path)

# Later, possibly with a newer version of scMaSigPro
ob <- updateObject(readRDS(path))
unlink(path)
```
