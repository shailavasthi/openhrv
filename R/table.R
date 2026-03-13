flatten_metrics <- function(x, prefix = NULL) {
  out <- list()
  for (nm in names(x)) {
    value <- x[[nm]]
    key <- if (is.null(prefix)) nm else paste(prefix, nm, sep = "_")
    if (is.list(value) && !is.data.frame(value)) {
      out <- c(out, flatten_metrics(value, prefix = key))
    } else if (length(value) > 1L && !is.null(names(value))) {
      named <- as.list(value)
      out <- c(out, flatten_metrics(named, prefix = key))
    } else {
      out[[key]] <- value
    }
  }
  out
}

pretty_metric_name <- function(x) {
  x <- gsub("^time_|^frequency_|^nonlinear_|^quality_|^derived_", "", x)
  x <- gsub("_", " ", x)
  tools::toTitleCase(x)
}

#' Convert metrics to a flat table
#'
#' @param x An `hrv_result` object.
#' @param domain Which domain to return.
#' @param shape Output shape, either `"wide"` or `"long"`.
#'
#' @return A `data.frame`.
#' @export
metrics_table <- function(x,
                          domain = c("all", "time", "frequency", "nonlinear", "quality"),
                          shape = c("wide", "long")) {
  if (!inherits(x, "hrv_result")) {
    stop("`x` must be an `hrv_result` object.", call. = FALSE)
  }
  domain <- match.arg(domain)
  shape <- match.arg(shape)
  source <- switch(
    domain,
    all = c(x$metrics, list(quality = x$preprocessing$quality)),
    time = list(time = x$metrics$time),
    frequency = list(frequency = x$metrics$frequency),
    nonlinear = list(nonlinear = x$metrics$nonlinear),
    quality = list(quality = x$preprocessing$quality)
  )
  flat <- flatten_metrics(source)
  wide <- as.data.frame(flat, check.names = FALSE)
  if (shape == "wide") {
    return(wide)
  }

  data.frame(
    metric = pretty_metric_name(names(flat)),
    value = unname(unlist(flat)),
    row.names = NULL,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}
