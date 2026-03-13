.openhrv_defaults <- list(
  unit = "ms",
  histogram_bin_ms = 7.8125,
  preprocessing = list(
    artifact_method = "automatic",
    detrend = "smoothness_priors",
    lambda = 500,
    interpolate = "cubic_spline",
    resample_hz = 4
  ),
  spectrum = list(
    method = "welch",
    bands = c(vlf_low = 0.00, vlf_high = 0.04,
              lf_low = 0.04, lf_high = 0.15,
              hf_low = 0.15, hf_high = 0.40),
    ar_order = 16
  ),
  nonlinear = list(
    entropy_m = 2,
    entropy_r = 0.2,
    mse_scales = 1:20,
    dfa_short = 4:16,
    dfa_long = 16:64,
    corr_dim_m = 10,
    recurrence_m = 10,
    recurrence_lag = 1,
    recurrence_lmin = 2
  ),
  record = list(
    short_term_minutes = 20,
    segment_minutes = 5
  )
)

openhrv_defaults <- function() {
  .openhrv_defaults
}
