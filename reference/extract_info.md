# Bins per path for the printed summary

`extract_info()` gives the mean bin size or the number of bins of each
path for
[`.ScMaSigPro_show()`](https://spriyansh.github.io/scMaSigPro/reference/dot-ScMaSigPro_show.md).

## Usage

``` r
extract_info(data, return_type = "avg_bin_size", bin_size_col, path_col)
```

## Arguments

- data:

  The bin metadata.

- return_type:

  `"avg_bin_size"` or `"num_bins"`.

- bin_size_col:

  The column with the number of cells of each bin.

- path_col:

  The column with the path labels.

## Value

One value per path: the mean bin size (`"avg_bin_size"`) or the number
of bins (`"num_bins"`, a table).

## See also

[ScMaSigPro](https://spriyansh.github.io/scMaSigPro/reference/ScMaSigPro-class.md),
whose `show()` method prints it.
