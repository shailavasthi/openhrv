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

test_that("automatic preprocessing identifies Kubios-style beat classes", {
  rr_extra <- c(rep(1000, 20), 420, 590, rep(1000, 20))
  prep_extra <- preprocess_rr(hrv_record(rr_extra))
  expect_true("extra" %in% prep_extra$flags)

  rr_missed <- c(rep(1000, 20), 1980, rep(1000, 20))
  prep_missed <- preprocess_rr(hrv_record(rr_missed))
  expect_true("missed" %in% prep_missed$flags)

  rr_ectopic <- c(rep(1000, 20), 760, 1240, rep(1000, 20))
  prep_ectopic <- preprocess_rr(hrv_record(rr_ectopic))
  expect_true(any(prep_ectopic$flags %in% c("ectopic", "short", "long")))
})
