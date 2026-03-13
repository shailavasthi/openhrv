artifact_classifier <- function(rr_ms) {
  baseline <- median_filter(rr_ms, k = 11L)
  spread <- mad_filter(rr_ms, k = 11L)
  spread[spread < 1e-6] <- stats::median(rr_ms) * 0.02
  ratio <- rr_ms / baseline
  drr <- c(0, diff(rr_ms))
  ddrr <- c(0, diff(drr))
  types <- rep("normal", length(rr_ms))

  short_idx <- which(ratio < 0.8 | rr_ms < baseline - 3 * spread)
  long_idx <- which(ratio > 1.2 | rr_ms > baseline + 3 * spread)
  ectopic_idx <- which(abs(drr) > 3 * stats::median(abs(drr - stats::median(drr))) &
                         abs(ddrr) > stats::median(abs(ddrr)) * 2)

  types[short_idx] <- "short"
  types[long_idx] <- "long"
  types[ectopic_idx] <- "ectopic"

  missed_idx <- which(ratio > 1.8)
  extra_idx <- which(ratio < 0.65)
  types[missed_idx] <- "missed"
  types[extra_idx] <- "extra"

  types[seq_len(min(2L, length(types)))] <- ifelse(types[seq_len(min(2L, length(types)))] == "normal",
                                                    "normal",
                                                    types[seq_len(min(2L, length(types)))])
  types[length(types)] <- ifelse(types[length(types)] == "normal", "normal", types[length(types)])
  types
}

correct_rr <- function(time_seconds, rr_ms, flags) {
  corrected <- rr_ms
  artifact_idx <- which(flags != "normal")
  if (!length(artifact_idx)) {
    return(corrected)
  }

  normal_idx <- which(flags == "normal")
  if (length(normal_idx) < 2L) {
    corrected[] <- stats::median(rr_ms)
    return(corrected)
  }

  interp <- stats::approx(
    x = time_seconds[normal_idx],
    y = rr_ms[normal_idx],
    xout = time_seconds[artifact_idx],
    method = "linear",
    rule = 2
  )
  corrected[artifact_idx] <- interp$y
  corrected
}

quality_summary <- function(flags) {
  tab <- table(factor(flags, levels = c("normal", "ectopic", "short", "long", "missed", "extra")))
  counts <- as.list(as.integer(tab))
  names(counts) <- names(tab)
  counts$artifact_total <- sum(unlist(counts[names(counts) != "normal"]))
  counts$artifact_fraction <- counts$artifact_total / length(flags)
  counts
}

#' Preprocess RR intervals
#'
#' @param x An `hrv_record` object.
#' @param artifact_method Artifact handling method.
#' @param detrend Detrending method.
#' @param lambda Smoothness priors lambda.
#' @param interpolate Interpolation method for evenly sampled tachograms.
#' @param resample_hz Interpolation frequency in Hz.
#'
#' @return An object of class `hrv_preprocessed`.
#' @export
preprocess_rr <- function(x,
                          artifact_method = c("automatic", "none"),
                          detrend = c("smoothness_priors", "none"),
                          lambda = 500,
                          interpolate = c("cubic_spline", "none"),
                          resample_hz = 4) {
  if (!inherits(x, "hrv_record")) {
    stop("`x` must be an `hrv_record` object.", call. = FALSE)
  }
  artifact_method <- match.arg(artifact_method)
  detrend <- match.arg(detrend)
  interpolate <- match.arg(interpolate)

  rr_ms <- x$rr_ms
  time_seconds <- x$time
  flags <- rep("normal", length(rr_ms))
  if (artifact_method == "automatic") {
    flags <- artifact_classifier(rr_ms)
  }
  corrected_rr <- correct_rr(time_seconds, rr_ms, flags)

  interp <- list(time = numeric(), value = numeric())
  if (interpolate == "cubic_spline") {
    interp <- interp_regular(time_seconds, corrected_rr, hz = resample_hz)
  }
  detrended <- corrected_rr
  detrended_interpolated <- interp$value
  if (detrend == "smoothness_priors") {
    detrended <- smoothness_priors(corrected_rr, lambda = lambda)
    if (length(interp$value)) {
      detrended_interpolated <- smoothness_priors(interp$value, lambda = lambda)
    }
  }

  structure(
    list(
      raw_rr_ms = rr_ms,
      corrected_rr_ms = corrected_rr,
      detrended_rr_ms = detrended,
      time = time_seconds,
      interpolated_time = interp$time,
      interpolated_rr_ms = interp$value,
      detrended_interpolated_rr_ms = detrended_interpolated,
      flags = flags,
      quality = quality_summary(flags),
      metadata = x$metadata,
      settings = list(
        artifact_method = artifact_method,
        detrend = detrend,
        lambda = lambda,
        interpolate = interpolate,
        resample_hz = resample_hz
      )
    ),
    class = "hrv_preprocessed"
  )
}

#' @export
print.hrv_preprocessed <- function(x, ...) {
  cat("<hrv_preprocessed>\n", sep = "")
  cat("  Beats:", length(x$raw_rr_ms), "\n")
  cat("  Artifacts:", x$quality$artifact_total, "\n")
  cat("  Artifact fraction:", sprintf("%.2f%%", 100 * x$quality$artifact_fraction), "\n")
  invisible(x)
}

#' @export
summary.hrv_preprocessed <- function(object, ...) {
  list(
    beats = length(object$raw_rr_ms),
    duration_minutes = duration_minutes(object$time),
    quality = object$quality,
    settings = object$settings
  )
}
