test_that("time-domain metrics are computed for a stable rhythm", {
  rr <- rep(1000, 300)
  res <- analyze_hrv(hrv_record(rr), domains = "time")
  tbl <- metrics_table(res, "time")

  expect_equal(tbl$time_mean_nn, 1000)
  expect_equal(tbl$time_mean_hr, 60)
  expect_equal(tbl$time_rmssd, 0)
  expect_equal(tbl$time_sdnn, 0)
})

test_that("long metric tables are available for reports", {
  rr <- rep(1000, 300)
  res <- analyze_hrv(hrv_record(rr), domains = "time")
  tbl <- metrics_table(res, "time", shape = "long")

  expect_true(all(c("metric", "value") %in% names(tbl)))
  expect_gt(nrow(tbl), 0)
})
