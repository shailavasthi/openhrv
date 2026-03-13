#' Construct an HRV record
#'
#' @param rr Numeric vector of RR or NN intervals.
#' @param time Optional numeric vector of cumulative beat timestamps in seconds.
#' @param unit Interval unit, either `"ms"` or `"s"`.
#' @param metadata Named list of optional metadata.
#'
#' @return An object of class `hrv_record`.
#' @export
hrv_record <- function(rr, time = NULL, unit = c("ms", "s"), metadata = list()) {
  unit <- match.arg(unit)
  assert_numeric_vector(rr, "rr")
  metadata <- validate_metadata(metadata)
  rr_ms <- rr_to_ms(as.numeric(rr), unit = unit)
  if (any(rr_ms <= 0)) {
    stop("`rr` must contain positive intervals.", call. = FALSE)
  }
  if (!is.null(time)) {
    assert_numeric_vector(time, "time")
    if (length(time) != length(rr_ms)) {
      stop("`time` must have the same length as `rr`.", call. = FALSE)
    }
    if (any(diff(time) <= 0)) {
      stop("`time` must be strictly increasing.", call. = FALSE)
    }
    time_seconds <- as.numeric(time)
  } else {
    time_seconds <- cumsum(rr_ms) / 1000
  }

  structure(
    list(
      rr_ms = rr_ms,
      time = time_seconds,
      metadata = metadata,
      settings = list(unit = unit)
    ),
    class = "hrv_record"
  )
}

#' @export
print.hrv_record <- function(x, ...) {
  cat("<hrv_record>\n", sep = "")
  cat("  Beats:", length(x$rr_ms), "\n")
  cat("  Duration (min):", sprintf("%.2f", duration_minutes(x$time)), "\n")
  invisible(x)
}
