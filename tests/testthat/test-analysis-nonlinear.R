test_that("nonlinear analysis returns poincare and entropy metrics", {
  rr <- 1000 + cumsum(rnorm(600, sd = 8))
  res <- analyze_hrv(hrv_record(rr), domains = "nonlinear")
  tbl <- metrics_table(res, "nonlinear")

  expect_true(all(c("nonlinear_sd1", "nonlinear_sd2", "nonlinear_sample_entropy") %in% names(tbl)))
  expect_true(is.finite(tbl$nonlinear_sd1))
  expect_true(is.finite(tbl$nonlinear_sd2))
})
