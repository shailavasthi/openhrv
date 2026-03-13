test_that("frequency-domain analysis returns expected fields", {
  t <- seq_len(600)
  rr <- 1000 + 80 * sin(2 * pi * 0.1 * t) + 40 * sin(2 * pi * 0.25 * t)
  res <- analyze_hrv(hrv_record(rr), domains = "frequency", spectrum_method = "welch")
  tbl <- metrics_table(res, "frequency")

  expect_true(all(c("frequency_lf_power", "frequency_hf_power", "frequency_lf_hf_ratio") %in% names(tbl)))
  expect_true(is.finite(tbl$frequency_lf_power))
  expect_true(is.finite(tbl$frequency_hf_power))
})

test_that("spectrum autoplot returns a ggplot object", {
  t <- seq_len(600)
  rr <- 1000 + 80 * sin(2 * pi * 0.1 * t) + 40 * sin(2 * pi * 0.25 * t)
  res <- analyze_hrv(hrv_record(rr), domains = "frequency", spectrum_method = "welch")
  p <- ggplot2::autoplot(res, type = "spectrum")

  expect_s3_class(p, "ggplot")
})
