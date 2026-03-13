test_that("derived metrics incorporate metadata when available", {
  rr <- 1000 + 30 * sin(seq(0, 8 * pi, length.out = 300))
  rec <- hrv_record(
    rr,
    metadata = list(
      duration_minutes = 30,
      avg_hr = 145,
      resting_hr = 55,
      max_hr = 190,
      weight_kg = 70,
      sex = "male"
    )
  )
  res <- analyze_hrv(rec)
  tbl <- metrics_table(res)

  expect_true(is.finite(tbl$derived_stress_index))
  expect_true(is.finite(tbl$derived_trimp))
})
