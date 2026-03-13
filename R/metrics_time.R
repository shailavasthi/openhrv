tinn_estimate <- function(rr_ms, bin_ms = openhrv_defaults()$histogram_bin_ms) {
  h <- mode_bin(rr_ms, binwidth = bin_ms)$hist
  nonzero <- which(h$counts > 0)
  if (length(nonzero) < 2L) {
    return(NA_real_)
  }
  min(h$breaks[max(1L, min(nonzero)) + 1L], h$breaks[max(nonzero) + 1L]) -
    h$breaks[min(nonzero)]
}

triangular_index <- function(rr_ms, bin_ms = openhrv_defaults()$histogram_bin_ms) {
  h <- mode_bin(rr_ms, binwidth = bin_ms)$hist
  if (!length(h$counts) || max(h$counts) == 0) {
    return(NA_real_)
  }
  length(rr_ms) / max(h$counts)
}

segment_metric <- function(rr_ms, time_seconds, fun, minutes = openhrv_defaults()$record$segment_minutes) {
  idx <- segment_index(time_seconds, minutes = minutes)
  pieces <- split(rr_ms, idx)
  vals <- vapply(pieces, fun, numeric(1))
  vals[is.finite(vals)]
}

time_domain_metrics <- function(rr_ms, time_seconds, recording_type) {
  drr <- diff(rr_ms)
  hr <- hr_from_rr(rr_ms)
  sdann <- NA_real_
  sdnni <- NA_real_
  if (recording_type == "long_term" && duration_minutes(time_seconds) >= 10) {
    mean_segments <- segment_metric(rr_ms, time_seconds, mean)
    sd_segments <- segment_metric(rr_ms, time_seconds, safe_sd)
    sdann <- safe_sd(mean_segments)
    sdnni <- mean(sd_segments, na.rm = TRUE)
  }

  list(
    mean_nn = mean(rr_ms),
    mean_hr = mean(hr),
    min_hr = min(hr),
    max_hr = max(hr),
    sdnn = safe_sd(rr_ms),
    rmssd = sqrt(mean(drr^2)),
    sdsd = safe_sd(drr),
    nn50 = sum(abs(drr) > 50),
    pnn50 = mean(abs(drr) > 50) * 100,
    nn20 = sum(abs(drr) > 20),
    pnn20 = mean(abs(drr) > 20) * 100,
    hrv_triangular_index = triangular_index(rr_ms),
    tinn = tinn_estimate(rr_ms),
    sdann = sdann,
    sdnni = sdnni
  )
}

stress_index_metric <- function(rr_ms) {
  mode_info <- mode_bin(rr_ms, openhrv_defaults()$histogram_bin_ms)
  mo <- mode_info$midpoint / 1000
  amo <- 100 * mode_info$amplitude / length(rr_ms)
  mxdmn <- (max(rr_ms) - min(rr_ms)) / 1000
  if (mo == 0 || mxdmn == 0) {
    return(NA_real_)
  }
  amo / (2 * mo * mxdmn)
}

prsa_capacity <- function(rr_ms, mode = c("acceleration", "deceleration")) {
  mode <- match.arg(mode)
  drr <- diff(rr_ms)
  anchors <- if (mode == "acceleration") which(drr < 0) + 1L else which(drr > 0) + 1L
  anchors <- anchors[anchors > 2L & anchors < length(rr_ms)]
  if (!length(anchors)) {
    return(NA_real_)
  }
  windows <- vapply(
    anchors,
    function(idx) {
      (rr_ms[idx] + rr_ms[idx + 1L] - rr_ms[idx - 1L] - rr_ms[idx - 2L]) / 4
    },
    numeric(1)
  )
  mean(windows, na.rm = TRUE)
}

exercise_metrics <- function(metadata) {
  required <- c("duration_minutes", "avg_hr", "resting_hr", "max_hr")
  if (!all(required %in% names(metadata))) {
    return(list(trimp = NA_real_, energy_kcal = NA_real_))
  }
  hr_ratio <- (metadata$avg_hr - metadata$resting_hr) /
    max(1, metadata$max_hr - metadata$resting_hr)
  sex_factor <- if (!is.null(metadata$sex) && tolower(metadata$sex) %in% c("female", "f")) 1.67 else 1.92
  trimp <- metadata$duration_minutes * hr_ratio * 0.64 * exp(sex_factor * hr_ratio)
  energy_kcal <- if (!is.null(metadata$weight_kg)) {
    metadata$duration_minutes * metadata$weight_kg * max(metadata$avg_hr / metadata$max_hr, 0)
  } else {
    NA_real_
  }
  list(trimp = trimp, energy_kcal = energy_kcal)
}
