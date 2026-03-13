test_that("hrv_record validates input and preprocesses RR intervals", {
  rr <- c(rep(1000, 20), 400, 1800, rep(1000, 20))
  rec <- hrv_record(rr)
  expect_s3_class(rec, "hrv_record")

  prep <- preprocess_rr(rec)
  expect_s3_class(prep, "hrv_preprocessed")
  expect_length(prep$flags, length(rr))
  expect_gt(prep$quality$artifact_total, 0)
})

test_that("hrv_record rejects invalid vectors", {
  expect_error(hrv_record(c(1000, NA_real_, 900)))
  expect_error(hrv_record(c(1000, -1, 900)))
})
