# openhrv

`openhrv` is an open R package for heart rate variability analysis from RR or NN intervals. It provides preprocessing, time-domain metrics, frequency-domain metrics, nonlinear metrics, plotting helpers, and a lightweight report generator.

The implementation is designed to be transparent and literature-aligned rather than a byte-for-byte clone of any commercial software.

## Install from a local tarball

```r
install.packages("openhrv_0.1.0.tar.gz", repos = NULL, type = "source")
```

## Install from GitHub

```r
install.packages("remotes")
remotes::install_github("YOUR_GITHUB_USERNAME/openhrv")
```

## Basic usage

```r
library(openhrv)

rr <- 1000 + 50 * sin(seq(0, 8 * pi, length.out = 300))
rec <- hrv_record(rr, unit = "ms")
prep <- preprocess_rr(rec)
res <- analyze_hrv(prep)

metrics_table(res, "time")
ggplot2::autoplot(res, type = "spectrum")
```
