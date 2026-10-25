# Number of bins of a histogram rule

`estBinSize()` gives the number of bins of one of the histogram rules
"Sturges", "Sqrt", "Rice", "Doane", "Freedman.Diaconis" and
"Scott.Normal" for a pseudotime vector, multiplied by `drop_fac`.

## Usage

``` r
estBinSize(time_vector, nPoints, drop_fac, bin_method)
```

## Arguments

- time_vector:

  The pseudotime values to bin.

- nPoints:

  The number of values of `time_vector`.

- drop_fac:

  The factor that multiplies the number of bins of the rule; values
  below 1 give fewer, wider bins.

- bin_method:

  The rule (see Details).

## Value

The number of bins of the rule times `drop_fac`, not rounded;
[`sc.squeeze()`](https://spriyansh.github.io/scMaSigPro/reference/sc.squeeze.md)
rounds it up.

## Details

With n the number of points (`nPoints`), the rules give:

- "Sturges":

  log2(n) + 1 bins (Sturges 1926). Srivastava et al. (2024),
  Supplementary Section S1.1, writes the number of bins as \\B =
  \[(\log_2(N)+1) \times k\]\\, with k the drop factor of their Section
  2.1, and every other rule the same way, with the brackets around the
  whole product: the rule times `drop_fac` is rounded up.

- "Sqrt":

  sqrt(n) bins, a common rule of thumb.

- "Rice":

  2 n^(1/3) bins (Lane and Scott), close to the oversmoothed number of
  bins (2n)^(1/3) of Terrell and Scott (1985).

- "Doane":

  1 + log2(n) + log2(1 + \|g\| / s) bins, with g the sample skewness of
  the points and s = sqrt(6 (n - 2) / ((n + 1) (n + 3))) (Doane 1976).

- "Freedman.Diaconis":

  bins of width 2 IQR / n^(1/3), with IQR the interquartile range of the
  points (Freedman and Diaconis 1981).

- "Scott.Normal":

  bins of width 3.49 sd / n^(1/3), with sd the standard deviation of the
  points (Scott 1979).

The widths of Freedman-Diaconis and Scott are turned into a number of
bins by dividing the range of the points by them. A width of 0 (all
points equal, or for Freedman-Diaconis equal first and third quartiles)
or an undefined width (Scott with a single point) gives one bin, with a
message.

## References

Sturges HA (1926). The choice of a class interval. Journal of the
American Statistical Association 21(153), 65-66.
[doi:10.1080/01621459.1926.10502161](https://doi.org/10.1080/01621459.1926.10502161)

Doane DP (1976). Aesthetic frequency classifications. The American
Statistician 30(4), 181-183.
[doi:10.1080/00031305.1976.10479172](https://doi.org/10.1080/00031305.1976.10479172)

Scott DW (1979). On optimal and data-based histograms. Biometrika 66(3),
605-610.
[doi:10.1093/biomet/66.3.605](https://doi.org/10.1093/biomet/66.3.605)

Freedman D, Diaconis P (1981). On the histogram as a density estimator:
L2 theory. Zeitschrift für Wahrscheinlichkeitstheorie und verwandte
Gebiete 57(4), 453-476.
[doi:10.1007/BF01025868](https://doi.org/10.1007/BF01025868)

Terrell GR, Scott DW (1985). Oversmoothed nonparametric density
estimates. Journal of the American Statistical Association 80(389),
209-214.
[doi:10.1080/01621459.1985.10477163](https://doi.org/10.1080/01621459.1985.10477163)

Lane DM, Scott D. Histograms. In Online Statistics Education: A
Multimedia Course of Study, Rice University.
<https://onlinestatbook.com/2/graphing_distributions/histograms.html>

Srivastava P, Benegas Coll M, Götz S, Nueda MJ, Conesa A (2024).
scMaSigPro: differential expression analysis along single-cell
trajectories. Bioinformatics 40(7), btae443.
[doi:10.1093/bioinformatics/btae443](https://doi.org/10.1093/bioinformatics/btae443)

## See also

Other binning helpers:
[`calc_bin_size()`](https://spriyansh.github.io/scMaSigPro/reference/calc_bin_size.md),
[`extract_interval()`](https://spriyansh.github.io/scMaSigPro/reference/extract_interval.md),
[`optimize_bin_max()`](https://spriyansh.github.io/scMaSigPro/reference/optimize_bin_max.md),
[`pb_counts()`](https://spriyansh.github.io/scMaSigPro/reference/pb_counts.md)

## Author

Priyansh Srivastava <spriyansh29@gmail.com>
