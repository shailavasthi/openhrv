coerce_preprocessed <- function(x) {
  if (inherits(x, "hrv_preprocessed")) {
    return(x)
  }
  if (inherits(x, "hrv_record")) {
    return(preprocess_rr(x))
  }
  stop("`x` must be an `hrv_record` or `hrv_preprocessed` object.", call. = FALSE)
}

duration_gate <- function(minutes, recording_type) {
  list(
    long_term = recording_type == "long_term" && minutes >= 10,
    nonlinear = minutes >= 1,
    frequency = minutes >= 1
  )
}

#' Analyze heart rate variability
#'
#' @param x An `hrv_record` or `hrv_preprocessed` object.
#' @param domains Domains to compute.
#' @param spectrum_method Spectrum estimator.
#' @param recording_type Recording type.
#'
#' @return An object of class `hrv_result`.
#' @export
analyze_hrv <- function(x,
                        domains = c("time", "frequency", "nonlinear"),
                        spectrum_method = c("welch", "ar", "lomb"),
                        recording_type = c("auto", "short_term", "long_term")) {
  prep <- coerce_preprocessed(x)
  spectrum_method <- match.arg(spectrum_method)
  recording_type <- match.arg(recording_type)
  recording_type <- if (recording_type == "auto") estimate_recording_type(prep$time) else recording_type
  domains <- unique(match.arg(domains, c("time", "frequency", "nonlinear"), several.ok = TRUE))
  gates <- duration_gate(duration_minutes(prep$time), recording_type = recording_type)

  metrics <- list()
  spectra <- NULL
  if ("time" %in% domains) {
    metrics$time <- time_domain_metrics(prep$corrected_rr_ms, prep$time, recording_type)
  }
  if ("frequency" %in% domains) {
    if (!gates$frequency) {
      warning("Frequency-domain metrics require at least 1 minute of data.", call. = FALSE)
      metrics$frequency <- list(
        total_power = NA_real_,
        vlf_power = NA_real_,
        lf_power = NA_real_,
        hf_power = NA_real_,
        lf_nu = NA_real_,
        hf_nu = NA_real_,
        lf_hf_ratio = NA_real_,
        vlf_peak = NA_real_,
        lf_peak = NA_real_,
        hf_peak = NA_real_
      )
    } else {
      spectrum <- frequency_domain_metrics(
        time_seconds = prep$time,
        rr_ms = prep$detrended_rr_ms,
        interpolated_time = prep$interpolated_time,
        interpolated_rr_ms = prep$detrended_interpolated_rr_ms,
        spectrum_method = spectrum_method,
        fs = prep$settings$resample_hz
      )
      metrics$frequency <- spectrum$metrics
      spectra <- spectrum$spectrum
    }
  }
  if ("nonlinear" %in% domains) {
    if (!gates$nonlinear) {
      warning("Nonlinear metrics require at least 1 minute of data.", call. = FALSE)
      metrics$nonlinear <- list()
    } else {
      metrics$nonlinear <- nonlinear_metrics(prep$corrected_rr_ms)
    }
  }

  metrics$derived <- c(
    list(
      stress_index = stress_index_metric(prep$corrected_rr_ms),
      acceleration_capacity = prsa_capacity(prep$corrected_rr_ms, mode = "acceleration"),
      deceleration_capacity = prsa_capacity(prep$corrected_rr_ms, mode = "deceleration")
    ),
    exercise_metrics(prep$metadata)
  )

  structure(
    list(
      preprocessing = prep,
      metrics = metrics,
      spectrum = spectra,
      settings = list(
        domains = domains,
        spectrum_method = spectrum_method,
        recording_type = recording_type
      )
    ),
    class = "hrv_result"
  )
}

#' @export
print.hrv_result <- function(x, ...) {
  cat("<hrv_result>\n", sep = "")
  cat("  Domains:", paste(x$settings$domains, collapse = ", "), "\n")
  if (!is.null(x$metrics$time)) {
    cat("  SDNN:", sprintf("%.2f", x$metrics$time$sdnn), "ms\n")
    cat("  RMSSD:", sprintf("%.2f", x$metrics$time$rmssd), "ms\n")
  }
  invisible(x)
}

#' @export
summary.hrv_result <- function(object, ...) {
  object$metrics
}
