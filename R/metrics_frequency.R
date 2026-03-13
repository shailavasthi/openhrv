welch_spectrum <- function(x, fs) {
  n <- length(x)
  if (n < 16L) {
    return(list(freq = numeric(), power = numeric()))
  }
  seg_length <- min(256L, 2L ^ floor(log2(max(16, n / 4))))
  step <- max(8L, seg_length %/% 2L)
  starts <- seq.int(1L, max(1L, n - seg_length + 1L), by = step)
  window <- 0.54 - 0.46 * cos(2 * pi * seq(0, seg_length - 1L) / (seg_length - 1L))
  scale <- fs * sum(window^2)
  spectra <- lapply(starts, function(s) {
    segment <- x[s:(s + seg_length - 1L)]
    segment <- segment - mean(segment)
    fft_vals <- stats::fft(segment * window)
    power <- (Mod(fft_vals)^2) / scale
    power[1:(seg_length %/% 2L + 1L)]
  })
  avg_power <- Reduce(`+`, spectra) / length(spectra)
  freq <- seq(0, fs / 2, length.out = length(avg_power))
  list(freq = freq, power = avg_power)
}

ar_spectrum <- function(x, fs, order = openhrv_defaults()$spectrum$ar_order) {
  fit <- stats::ar(x, aic = FALSE, order.max = order, method = "yule-walker")
  freq <- seq(0, fs / 2, length.out = 512)
  omega <- 2 * pi * freq / fs
  den <- vapply(
    omega,
    function(w) {
      z <- exp(-1i * w * seq_along(fit$ar))
      Mod(1 - sum(fit$ar * z))^2
    },
    numeric(1)
  )
  power <- fit$var.pred / den
  list(freq = freq, power = power)
}

lomb_spectrum <- function(time_seconds, x, freq_max = 0.5, n_freq = 512L) {
  freq <- seq(0.001, freq_max, length.out = n_freq)
  x <- x - mean(x)
  power <- vapply(freq, function(f) {
    w <- 2 * pi * f
    tau <- atan2(sum(sin(2 * w * time_seconds)), sum(cos(2 * w * time_seconds))) / (2 * w)
    cterm <- cos(w * (time_seconds - tau))
    sterm <- sin(w * (time_seconds - tau))
    ((sum(x * cterm)^2) / sum(cterm^2) + (sum(x * sterm)^2) / sum(sterm^2)) / (2 * stats::var(x))
  }, numeric(1))
  list(freq = freq, power = power)
}

frequency_domain_metrics <- function(time_seconds,
                                     rr_ms,
                                     interpolated_time,
                                     interpolated_rr_ms,
                                     spectrum_method = c("welch", "ar", "lomb"),
                                     bands = openhrv_defaults()$spectrum$bands,
                                     fs = openhrv_defaults()$preprocessing$resample_hz) {
  spectrum_method <- match.arg(spectrum_method)
  spectrum <- switch(
    spectrum_method,
    welch = welch_spectrum(interpolated_rr_ms, fs = fs),
    ar = ar_spectrum(interpolated_rr_ms, fs = fs),
    lomb = lomb_spectrum(time_seconds, rr_ms)
  )

  freq <- spectrum$freq
  power <- spectrum$power
  if (!length(freq)) {
    return(list(
      metrics = list(
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
      ),
      spectrum = spectrum
    ))
  }

  vlf <- integrate_band(freq, power, bands["vlf_low"], bands["vlf_high"])
  lf <- integrate_band(freq, power, bands["lf_low"], bands["lf_high"])
  hf <- integrate_band(freq, power, bands["hf_low"], bands["hf_high"])
  total <- integrate_band(freq, power, bands["vlf_low"], bands["hf_high"])
  lf_hf_sum <- lf + hf
  peak_in_band <- function(lower, upper) {
    idx <- which(freq >= lower & freq < upper)
    if (!length(idx)) return(NA_real_)
    freq[idx[which.max(power[idx])]]
  }

  list(
    metrics = list(
      total_power = total,
      vlf_power = vlf,
      lf_power = lf,
      hf_power = hf,
      lf_nu = if (lf_hf_sum == 0) NA_real_ else (lf / lf_hf_sum) * 100,
      hf_nu = if (lf_hf_sum == 0) NA_real_ else (hf / lf_hf_sum) * 100,
      lf_hf_ratio = safe_ratio(lf, hf),
      vlf_peak = peak_in_band(bands["vlf_low"], bands["vlf_high"]),
      lf_peak = peak_in_band(bands["lf_low"], bands["lf_high"]),
      hf_peak = peak_in_band(bands["hf_low"], bands["hf_high"])
    ),
    spectrum = spectrum
  )
}
