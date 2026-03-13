`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

assert_numeric_vector <- function(x, name) {
  if (!is.numeric(x) || !is.vector(x)) {
    stop(sprintf("`%s` must be a numeric vector.", name), call. = FALSE)
  }
  if (length(x) < 3L) {
    stop(sprintf("`%s` must contain at least 3 observations.", name), call. = FALSE)
  }
  if (any(!is.finite(x))) {
    stop(sprintf("`%s` must not contain missing or infinite values.", name), call. = FALSE)
  }
}

validate_metadata <- function(metadata) {
  if (is.null(metadata)) {
    metadata <- list()
  }
  if (!is.list(metadata)) {
    stop("`metadata` must be a list.", call. = FALSE)
  }
  metadata
}

rr_to_ms <- function(rr, unit) {
  unit <- match.arg(unit, c("ms", "s"))
  if (unit == "s") rr * 1000 else rr
}

rr_to_seconds <- function(rr_ms) {
  rr_ms / 1000
}

rr_timestamps <- function(rr_ms, time = NULL) {
  if (!is.null(time)) {
    assert_numeric_vector(time, "time")
    return(as.numeric(time))
  }
  cumsum(rr_ms) / 1000
}

median_filter <- function(x, k = 11L) {
  k <- as.integer(max(3L, k))
  if (k %% 2L == 0L) {
    k <- k + 1L
  }
  half <- (k - 1L) / 2L
  out <- numeric(length(x))
  for (i in seq_along(x)) {
    lo <- max(1L, i - half)
    hi <- min(length(x), i + half)
    out[i] <- stats::median(x[lo:hi])
  }
  out
}

mad_filter <- function(x, k = 11L) {
  k <- as.integer(max(3L, k))
  if (k %% 2L == 0L) {
    k <- k + 1L
  }
  half <- (k - 1L) / 2L
  out <- numeric(length(x))
  for (i in seq_along(x)) {
    lo <- max(1L, i - half)
    hi <- min(length(x), i + half)
    out[i] <- stats::mad(x[lo:hi], center = stats::median(x[lo:hi]), constant = 1)
  }
  out
}

safe_sd <- function(x) {
  if (length(x) < 2L) return(NA_real_)
  stats::sd(x)
}

integrate_band <- function(freq, power, lower, upper) {
  idx <- which(freq >= lower & freq < upper)
  if (length(idx) < 2L) {
    return(0)
  }
  sum(diff(freq[idx]) * (power[idx[-1L]] + power[idx[-length(idx)]]) / 2)
}

duration_minutes <- function(time_seconds) {
  (max(time_seconds) - min(time_seconds)) / 60
}

segment_index <- function(time_seconds, minutes = 5) {
  floor((time_seconds - min(time_seconds)) / (minutes * 60)) + 1L
}

interp_regular <- function(time_seconds, values, hz = 4) {
  if (length(values) < 4L) {
    return(list(time = numeric(), value = numeric()))
  }
  new_time <- seq(min(time_seconds), max(time_seconds), by = 1 / hz)
  fun <- stats::splinefun(time_seconds, values, method = "natural")
  list(time = new_time, value = fun(new_time))
}

smoothness_priors <- function(x, lambda = 500) {
  n <- length(x)
  if (n < 3L) {
    return(rep(0, n))
  }
  d2 <- diff(diag(n), differences = 2)
  trend <- solve(diag(n) + (lambda^2) * crossprod(d2), x)
  x - trend
}

embed_series <- function(x, m, lag = 1L) {
  m <- as.integer(m)
  lag <- as.integer(lag)
  n <- length(x) - (m - 1L) * lag
  if (n <= 1L) {
    return(matrix(numeric(), nrow = 0L, ncol = m))
  }
  out <- matrix(NA_real_, nrow = n, ncol = m)
  for (i in seq_len(m)) {
    idx <- seq.int(1L + (i - 1L) * lag, length.out = n)
    out[, i] <- x[idx]
  }
  out
}

estimate_recording_type <- function(time_seconds) {
  if (duration_minutes(time_seconds) < openhrv_defaults()$record$short_term_minutes) {
    "short_term"
  } else {
    "long_term"
  }
}

safe_ratio <- function(x, y) {
  if (is.na(x) || is.na(y) || y == 0) {
    return(NA_real_)
  }
  x / y
}

hr_from_rr <- function(rr_ms) {
  60000 / rr_ms
}

mode_bin <- function(x, binwidth) {
  breaks <- seq(floor(min(x) / binwidth) * binwidth,
                ceiling(max(x) / binwidth) * binwidth + binwidth,
                by = binwidth)
  h <- graphics::hist(x, breaks = breaks, plot = FALSE)
  idx <- which.max(h$counts)
  list(
    midpoint = h$mids[idx],
    amplitude = h$counts[idx],
    hist = h
  )
}

utils::globalVariables(c(
  "time", "corrected_rr_ms", "flag", "x", "freq", "power", "fluctuation",
  "band", "xmin", "xmax", "cutoff", "y", "label"
))
