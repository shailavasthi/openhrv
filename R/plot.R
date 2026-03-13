plot_tachogram_data <- function(x) {
  data.frame(
    time = x$time,
    raw_rr_ms = x$raw_rr_ms,
    corrected_rr_ms = x$corrected_rr_ms,
    flag = x$flags
  )
}

#' @export
autoplot.hrv_preprocessed <- function(object, ...) {
  df <- plot_tachogram_data(object)
  ggplot2::ggplot(df, ggplot2::aes(x = time, y = corrected_rr_ms, colour = flag)) +
    ggplot2::geom_line(alpha = 0.6) +
    ggplot2::geom_point(size = 1.2) +
    ggplot2::labs(
      x = "Time (s)",
      y = "RR interval (ms)",
      title = "Preprocessed Tachogram",
      colour = "Beat flag"
    ) +
    ggplot2::theme_minimal()
}

poincare_data <- function(rr_ms) {
  data.frame(
    x = rr_ms[-length(rr_ms)],
    y = rr_ms[-1L]
  )
}

spectrum_band_data <- function(object) {
  freq_metrics <- object$metrics$frequency %||% list()
  data.frame(
    band = c("VLF", "LF", "HF"),
    xmin = c(0.00, 0.04, 0.15),
    xmax = c(0.04, 0.15, 0.40),
    power = c(
      freq_metrics$vlf_power %||% NA_real_,
      freq_metrics$lf_power %||% NA_real_,
      freq_metrics$hf_power %||% NA_real_
    ),
    fill = c("#9ecae1", "#a1d99b", "#fdae6b"),
    stringsAsFactors = FALSE
  )
}

report_metric_value <- function(x) {
  if (!is.numeric(x) || !is.finite(x)) {
    return(as.character(x))
  }
  format(round(x, 3), trim = TRUE, nsmall = 0)
}

#' @export
autoplot.hrv_result <- function(object, type = c("poincare", "spectrum", "dfa"), ...) {
  type <- match.arg(type)
  prep <- object$preprocessing
  if (type == "poincare") {
    df <- poincare_data(prep$corrected_rr_ms)
    return(
      ggplot2::ggplot(df, ggplot2::aes(x = x, y = y)) +
        ggplot2::geom_point(alpha = 0.5) +
        ggplot2::geom_abline(intercept = 0, slope = 1, linetype = 2) +
        ggplot2::labs(
          x = expression(RR[n] ~ "(ms)"),
          y = expression(RR[n + 1] ~ "(ms)"),
          title = "Poincare Plot"
        ) +
        ggplot2::theme_minimal()
    )
  }
  if (type == "spectrum") {
    if (is.null(object$spectrum)) {
      stop("Spectrum data are not available for this result.", call. = FALSE)
    }
    df <- data.frame(freq = object$spectrum$freq, power = object$spectrum$power)
    band_df <- spectrum_band_data(object)
    y_max <- max(df$power, na.rm = TRUE)
    label_df <- transform(
      band_df,
      x = (xmin + xmax) / 2,
      y = y_max * c(0.92, 0.82, 0.72),
      label = sprintf("%s\nPower: %s", band, vapply(power, report_metric_value, character(1)))
    )
    cutoff_df <- data.frame(cutoff = c(0.04, 0.15, 0.40))
    return(
      ggplot2::ggplot(df, ggplot2::aes(x = freq, y = power)) +
        ggplot2::geom_rect(
          data = band_df,
          ggplot2::aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf, fill = band),
          inherit.aes = FALSE,
          alpha = 0.18,
          colour = NA
        ) +
        ggplot2::geom_line() +
        ggplot2::geom_vline(
          data = cutoff_df,
          ggplot2::aes(xintercept = cutoff),
          linetype = 2,
          colour = "grey35"
        ) +
        ggplot2::geom_text(
          data = label_df,
          ggplot2::aes(x = x, y = y, label = label, colour = band),
          inherit.aes = FALSE,
          size = 3.3,
          fontface = "bold",
          show.legend = FALSE
        ) +
        ggplot2::scale_fill_manual(values = stats::setNames(band_df$fill, band_df$band)) +
        ggplot2::scale_colour_manual(values = stats::setNames(band_df$fill, band_df$band)) +
        ggplot2::coord_cartesian(xlim = c(0, 0.45)) +
        ggplot2::labs(
          x = "Frequency (Hz)",
          y = "Power",
          title = "HRV Spectrum",
          subtitle = sprintf(
            "Total power: %s",
            report_metric_value(object$metrics$frequency$total_power %||% NA_real_)
          ),
          fill = "Band"
        ) +
        ggplot2::theme_minimal()
    )
  }

  scales <- openhrv_defaults()$nonlinear$dfa_short
  y <- cumsum(prep$corrected_rr_ms - mean(prep$corrected_rr_ms))
  fluct <- vapply(scales, function(n) {
    segments <- floor(length(y) / n)
    rms <- vapply(seq_len(segments), function(i) {
      idx <- ((i - 1L) * n + 1L):(i * n)
      fit <- stats::lm(y[idx] ~ seq_along(idx))
      sqrt(mean(stats::residuals(fit)^2))
    }, numeric(1))
    sqrt(mean(rms^2))
  }, numeric(1))
  df <- data.frame(scale = scales, fluctuation = fluct)
  ggplot2::ggplot(df, ggplot2::aes(x = scale, y = fluctuation)) +
    ggplot2::geom_point() +
    ggplot2::geom_smooth(method = "lm", se = FALSE) +
    ggplot2::scale_x_log10() +
    ggplot2::scale_y_log10() +
    ggplot2::labs(x = "Scale", y = "Fluctuation", title = "DFA") +
    ggplot2::theme_minimal()
}
