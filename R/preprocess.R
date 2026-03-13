rolling_quantile <- function(x, probs, k = 91L) {
  k <- as.integer(max(5L, k))
  if (k %% 2L == 0L) {
    k <- k + 1L
  }
  half <- (k - 1L) / 2L
  out <- matrix(NA_real_, nrow = length(x), ncol = length(probs))
  for (i in seq_along(x)) {
    lo <- max(1L, i - half)
    hi <- min(length(x), i + half)
    out[i, ] <- stats::quantile(x[lo:hi], probs = probs, names = FALSE, type = 8)
  }
  out
}

adaptive_drr_threshold <- function(drr, k = 91L, scale = 5.2) {
  q <- rolling_quantile(abs(drr), probs = c(0.25, 0.75), k = k)
  qd <- (q[, 2L] - q[, 1L]) / 2
  floor_threshold <- stats::median(abs(drr)) * 0.1
  pmax(scale * qd, floor_threshold)
}

artifact_classifier <- function(rr_ms) {
  n <- length(rr_ms)
  if (n < 5L) {
    return(rep("normal", n))
  }

  med_rr <- median_filter(rr_ms, k = 11L)
  drr <- c(0, diff(rr_ms))
  threshold <- adaptive_drr_threshold(drr, k = 91L, scale = 5.2)
  types <- rep("normal", n)

  for (i in seq_len(n)) {
    local_rr <- med_rr[i]
    current_rr <- rr_ms[i]
    current_abs_drr <- abs(drr[i])
    next_drr <- if (i < n) drr[i + 1L] else 0
    prev_drr <- if (i > 1L) drr[i] else 0
    active_threshold <- threshold[i]
    next_threshold <- if (i < n) threshold[i + 1L] else active_threshold

    if (current_abs_drr <= active_threshold &&
        abs(next_drr) <= next_threshold &&
        current_rr > 0.75 * local_rr &&
        current_rr < 1.25 * local_rr) {
      next
    }

    if (i < n) {
      combined_rr <- current_rr + rr_ms[i + 1L]
      if (current_rr < 0.75 * local_rr &&
          abs(combined_rr - local_rr) <= 0.2 * local_rr) {
        types[i] <- "extra"
        next
      }
    }

    if (current_rr > 1.6 * local_rr) {
      half_rr <- current_rr / 2
      if (abs(half_rr - local_rr) <= 0.2 * local_rr) {
        types[i] <- "missed"
        next
      }
    }

    if (current_abs_drr > active_threshold && abs(next_drr) > next_threshold) {
      opposite_sign <- sign(drr[i]) != sign(next_drr) && sign(drr[i]) != 0 && sign(next_drr) != 0
      return_to_baseline <- if (i < (n - 1L)) {
        abs(rr_ms[i + 1L] - med_rr[i + 1L]) <= 0.2 * med_rr[i + 1L]
      } else {
        TRUE
      }
      if (opposite_sign && return_to_baseline) {
        types[i] <- "ectopic"
        next
      }
    }

    if (current_rr < 0.8 * local_rr || (current_abs_drr > active_threshold && current_rr < local_rr)) {
      types[i] <- "short"
      next
    }

    if (current_rr > 1.2 * local_rr || (current_abs_drr > active_threshold && current_rr > local_rr)) {
      types[i] <- "long"
    }
  }

  types[c(1L, n)] <- ifelse(types[c(1L, n)] == "normal", "normal", types[c(1L, n)])
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
