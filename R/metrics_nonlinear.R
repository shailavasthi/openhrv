approximate_entropy <- function(x, m = 2L, r = 0.2 * stats::sd(x)) {
  phi <- function(mm) {
    emb <- embed_series(x, mm, lag = 1L)
    if (!nrow(emb)) return(NA_real_)
    cm <- vapply(seq_len(nrow(emb)), function(i) {
      mean(apply(abs(t(emb) - emb[i, ]), 1L, max) <= r)
    }, numeric(1))
    mean(log(cm))
  }
  phi(m) - phi(m + 1L)
}

sample_entropy <- function(x, m = 2L, r = 0.2 * stats::sd(x)) {
  count_matches <- function(mm) {
    emb <- embed_series(x, mm, lag = 1L)
    if (!nrow(emb)) return(0)
    total <- 0
    for (i in seq_len(nrow(emb) - 1L)) {
      d <- apply(abs(emb[(i + 1L):nrow(emb), , drop = FALSE] - emb[i, ]), 1L, max)
      total <- total + sum(d <= r)
    }
    total
  }
  b <- count_matches(m)
  a <- count_matches(m + 1L)
  if (a == 0 || b == 0) {
    return(NA_real_)
  }
  -log(a / b)
}

multiscale_entropy <- function(x, scales = openhrv_defaults()$nonlinear$mse_scales,
                               m = 2L, r = 0.2 * stats::sd(x)) {
  out <- vapply(scales, function(scale) {
    chunks <- floor(length(x) / scale)
    if (chunks <= m + 1L) {
      return(NA_real_)
    }
    coarse <- vapply(seq_len(chunks), function(i) {
      mean(x[((i - 1L) * scale + 1L):(i * scale)])
    }, numeric(1))
    sample_entropy(coarse, m = m, r = r)
  }, numeric(1))
  stats::setNames(out, paste0("scale_", scales))
}

dfa_metric <- function(x, scales) {
  if (length(x) < max(scales) + 2L) {
    return(NA_real_)
  }
  y <- cumsum(x - mean(x))
  fluct <- vapply(scales, function(n) {
    segments <- floor(length(y) / n)
    if (segments < 2L) return(NA_real_)
    rms <- vapply(seq_len(segments), function(i) {
      idx <- ((i - 1L) * n + 1L):(i * n)
      fit <- stats::lm(y[idx] ~ seq_along(idx))
      sqrt(mean(stats::residuals(fit)^2))
    }, numeric(1))
    sqrt(mean(rms^2))
  }, numeric(1))
  valid <- which(is.finite(fluct) & fluct > 0)
  if (length(valid) < 2L) {
    return(NA_real_)
  }
  fit <- stats::lm(log10(fluct[valid]) ~ log10(scales[valid]))
  unname(stats::coef(fit)[2L])
}

correlation_dimension <- function(x, m = openhrv_defaults()$nonlinear$corr_dim_m) {
  emb <- embed_series(x, m = m, lag = 1L)
  n <- nrow(emb)
  if (n < 10L) {
    return(NA_real_)
  }
  d <- as.matrix(stats::dist(emb))
  d <- d[upper.tri(d)]
  radii <- exp(seq(log(stats::quantile(d, 0.1)), log(stats::quantile(d, 0.9)), length.out = 12L))
  corr <- vapply(radii, function(r) mean(d < r), numeric(1))
  valid <- which(corr > 0 & corr < 1)
  if (length(valid) < 2L) {
    return(NA_real_)
  }
  fit <- stats::lm(log(corr[valid]) ~ log(radii[valid]))
  unname(stats::coef(fit)[2L])
}

recurrence_metrics <- function(x,
                               m = openhrv_defaults()$nonlinear$recurrence_m,
                               lag = openhrv_defaults()$nonlinear$recurrence_lag,
                               radius = sqrt(openhrv_defaults()$nonlinear$recurrence_m) * stats::sd(x),
                               lmin = openhrv_defaults()$nonlinear$recurrence_lmin) {
  emb <- embed_series(x, m = m, lag = lag)
  n <- nrow(emb)
  if (n < 5L) {
    return(list(rr = NA_real_, det = NA_real_, lmean = NA_real_, lmax = NA_real_, shan = NA_real_))
  }
  dmat <- as.matrix(stats::dist(emb))
  rp <- dmat <= radius
  rr <- mean(rp)
  diag_lengths <- integer()
  for (k in seq.int(-(n - 1L), n - 1L)) {
    if (k >= 0L) {
      rows <- seq_len(n - k)
      cols <- rows + k
    } else {
      cols <- seq_len(n + k)
      rows <- cols - k
    }
    diag_vals <- rp[cbind(rows, cols)]
    runs <- rle(diag_vals)
    diag_lengths <- c(diag_lengths, runs$lengths[runs$values & runs$lengths >= lmin])
  }
  if (!length(diag_lengths)) {
    return(list(rr = rr, det = 0, lmean = 0, lmax = 0, shan = 0))
  }
  probs <- table(diag_lengths) / length(diag_lengths)
  list(
    rr = rr,
    det = sum(diag_lengths) / sum(rp),
    lmean = mean(diag_lengths),
    lmax = max(diag_lengths),
    shan = -sum(probs * log(probs))
  )
}

nonlinear_metrics <- function(rr_ms) {
  drr <- diff(rr_ms)
  sd1 <- sqrt(stats::var(drr) / 2)
  sd2 <- sqrt(2 * stats::var(rr_ms) - stats::var(drr) / 2)
  r <- openhrv_defaults()$nonlinear$entropy_r * safe_sd(rr_ms)
  mse <- multiscale_entropy(rr_ms, r = r)
  rqa <- recurrence_metrics(rr_ms)

  list(
    sd1 = sd1,
    sd2 = sd2,
    sd2_sd1_ratio = safe_ratio(sd2, sd1),
    approximate_entropy = approximate_entropy(rr_ms, m = openhrv_defaults()$nonlinear$entropy_m, r = r),
    sample_entropy = sample_entropy(rr_ms, m = openhrv_defaults()$nonlinear$entropy_m, r = r),
    mse = mse,
    dfa_alpha1 = dfa_metric(rr_ms, openhrv_defaults()$nonlinear$dfa_short),
    dfa_alpha2 = dfa_metric(rr_ms, openhrv_defaults()$nonlinear$dfa_long),
    correlation_dimension = correlation_dimension(rr_ms),
    recurrence_rate = rqa$rr,
    recurrence_determinism = rqa$det,
    recurrence_lmean = rqa$lmean,
    recurrence_lmax = rqa$lmax,
    recurrence_shannon_entropy = rqa$shan
  )
}
