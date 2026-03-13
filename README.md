# openhrv

`openhrv` is an open R package for heart rate variability analysis from RR or NN intervals. It is designed for transparent, literature-aligned HRV workflows in R, including preprocessing, time-domain metrics, frequency-domain metrics, nonlinear metrics, quick plots, and HTML reporting.

The package is intended to be methodologically open rather than a byte-for-byte clone of any commercial software.

## What the package does

`openhrv` supports a full RR-interval workflow:

- `hrv_record()`: validate and store RR intervals, optional timestamps, and metadata
- `preprocess_rr()`: flag likely artifacts, correct suspicious beats, interpolate, and detrend
- `analyze_hrv()`: compute time, frequency, nonlinear, and derived HRV metrics
- `metrics_table()`: export results as wide or long tables for downstream analysis
- `ggplot2::autoplot()`: generate tachogram, Poincare, spectrum, and DFA plots
- `hrv_report()`: render a simple HTML report with tables and plots

## Implemented feature areas

### Input handling

- RR or NN intervals in milliseconds or seconds
- Optional cumulative beat timestamps
- Optional metadata for exercise-derived metrics
- Validation of interval length, positivity, finiteness, and timestamp ordering

### Preprocessing

- Automatic artifact classification into normal, ectopic, short, long, missed, and extra beats
- Linear correction of flagged beats using neighboring normal intervals
- Optional cubic-spline interpolation to an evenly sampled tachogram
- Optional smoothness-priors detrending for spectral and nonlinear analysis
- Quality summary including artifact counts and artifact fraction

### Time-domain and geometric metrics

- Mean NN
- Mean HR
- Minimum HR
- Maximum HR
- SDNN
- RMSSD
- SDSD
- NN50 and pNN50
- NN20 and pNN20
- HRV triangular index
- TINN
- SDANN for longer recordings
- SDNNI for longer recordings

### Frequency-domain metrics

- Welch spectrum
- Autoregressive spectrum
- Lomb-Scargle style uneven-sampling spectrum
- Total power
- VLF power
- LF power
- HF power
- LF normalized units
- HF normalized units
- LF/HF ratio
- Peak frequency for VLF, LF, and HF bands

Default bands:

- VLF: `0.00` to `0.04` Hz
- LF: `0.04` to `0.15` Hz
- HF: `0.15` to `0.40` Hz

### Nonlinear metrics

- Poincare SD1
- Poincare SD2
- SD2/SD1 ratio
- Approximate entropy
- Sample entropy
- Multiscale entropy
- DFA alpha1
- DFA alpha2
- Correlation dimension estimate
- Recurrence rate
- Recurrence determinism
- Mean diagonal line length
- Maximum diagonal line length
- Recurrence Shannon entropy

### Derived metrics

- Stress index
- Acceleration capacity
- Deceleration capacity
- TRIMP when exercise metadata are available
- Simple energy expenditure estimate when exercise metadata are available

### Plots and reporting

- Preprocessed tachogram with beat flags
- Poincare plot
- HRV spectrum with shaded VLF/LF/HF bands, cutoff lines, and power annotations
- DFA plot
- HTML report with domain-specific metric tables and plots in a two-column layout

## Installation

### Install from a local tarball

```r
install.packages("openhrv_0.1.0.tar.gz", repos = NULL, type = "source")
```

### Install from GitHub

```r
install.packages("remotes")
remotes::install_github("YOUR_GITHUB_USERNAME/openhrv")
```

## Basic workflow

```r
library(openhrv)
library(ggplot2)

rr <- 1000 + 50 * sin(seq(0, 8 * pi, length.out = 300))

rec <- hrv_record(rr, unit = "ms")
prep <- preprocess_rr(rec)
res <- analyze_hrv(prep)

print(prep)
print(res)
```

## Input examples

### 1. RR intervals only

This is the most direct input format.

```r
rr_ms <- c(980, 1005, 995, 1010, 990, 1002)
rec <- hrv_record(rr_ms, unit = "ms")
```

### 2. RR intervals with cumulative beat times

If you already have timestamps aligned to the intervals, pass them explicitly.

```r
rr_sec <- c(0.98, 1.01, 0.99, 1.00, 1.02)
time_sec <- cumsum(rr_sec)

rec <- hrv_record(rr_sec, time = time_sec, unit = "s")
```

### 3. R-peak times converted to RR intervals

If your source data are absolute R-peak times, convert them before calling `hrv_record()`.

```r
r_peak_sec <- c(0.82, 1.81, 2.80, 3.79, 4.78, 5.79)
rr_sec <- diff(r_peak_sec)
time_sec <- r_peak_sec[-1]

rec <- hrv_record(rr_sec, time = time_sec, unit = "s")
```

### 4. Add metadata for derived exercise metrics

```r
rec <- hrv_record(
  rr = 1000 + 30 * sin(seq(0, 8 * pi, length.out = 300)),
  metadata = list(
    duration_minutes = 30,
    avg_hr = 145,
    resting_hr = 55,
    max_hr = 190,
    weight_kg = 70,
    sex = "male"
  )
)
```

## Preprocessing examples

### Default preprocessing

```r
prep <- preprocess_rr(rec)
```

This performs:

- automatic artifact screening
- beat correction
- cubic-spline interpolation
- smoothness-priors detrending

### Minimal preprocessing

Use this if you already trust the RR series and only want analysis.

```r
prep <- preprocess_rr(
  rec,
  artifact_method = "none",
  detrend = "none",
  interpolate = "none"
)
```

### Inspect preprocessing output

```r
prep$quality
head(prep$flags)
```

## Analysis examples

### Full analysis

```r
res <- analyze_hrv(prep)
```

### Time-domain only

```r
res_time <- analyze_hrv(prep, domains = "time")
```

### Frequency-domain only with AR spectrum

```r
res_freq <- analyze_hrv(
  prep,
  domains = "frequency",
  spectrum_method = "ar"
)
```

### Frequency-domain only with Lomb spectrum

```r
res_lomb <- analyze_hrv(
  prep,
  domains = "frequency",
  spectrum_method = "lomb"
)
```

### Force long-term interpretation

```r
res_long <- analyze_hrv(
  prep,
  recording_type = "long_term"
)
```

## Extracting metrics

### One-row wide table

This is useful for model inputs, spreadsheets, or combining many results.

```r
tbl <- metrics_table(res)
tbl
```

### Long table for a specific domain

This is better for reports and human-readable summaries.

```r
metrics_table(res, domain = "time", shape = "long")
metrics_table(res, domain = "frequency", shape = "long")
metrics_table(res, domain = "nonlinear", shape = "long")
metrics_table(res, domain = "quality", shape = "long")
```

## Plotting examples

`openhrv` uses the `ggplot2` `autoplot()` generic.

### Tachogram after preprocessing

```r
ggplot2::autoplot(prep)
```

### Poincare plot

```r
ggplot2::autoplot(res, type = "poincare")
```

### Frequency spectrum

```r
ggplot2::autoplot(res, type = "spectrum")
```

This plot includes:

- VLF, LF, and HF shaded bands
- band cutoff lines
- per-band power annotations
- total power in the subtitle

### DFA plot

```r
ggplot2::autoplot(res, type = "dfa")
```

## HTML report

Create an HTML summary with metrics on the left and plots on the right.

```r
path <- hrv_report(res)
browseURL(path)
```

The report contains:

- summary output
- separate tables for time, frequency, nonlinear, derived, and quality metrics
- Poincare plot
- spectrum plot
- DFA plot

## Example using included data

```r
df <- read.csv(system.file("extdata", "resting-clean-5min.csv", package = "openhrv"))

rec <- hrv_record(df$rr_ms)
prep <- preprocess_rr(rec)
res <- analyze_hrv(prep)

metrics_table(res, "time", shape = "long")
ggplot2::autoplot(res, type = "spectrum")
```

## Included example files

- `resting-clean-5min.csv`: clean short resting RR series
- `resting-artifacts-5min.csv`: short RR series with obvious artifacts
- `long-recording-60min.csv`: long RR series for long-term metrics

## Notes and limitations

- The package starts from RR or NN intervals, not raw ECG or PPG waveforms.
- Artifact correction is transparent and open, but not intended as an exact replication of proprietary internal implementations.
- Some nonlinear measures are implementation-sensitive and should be interpreted as literature-aligned estimates.
- `testthat` is listed in `Suggests`; if it is not installed, package checks can skip the formal test suite.

## Updating from GitHub

```r
install.packages("remotes")
remotes::install_github("YOUR_GITHUB_USERNAME/openhrv", force = TRUE)
```
