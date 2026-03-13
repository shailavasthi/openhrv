set.seed(42)

clean_rr <- 1000 + 40 * sin(seq(0, 12 * pi, length.out = 300)) + rnorm(300, sd = 12)
artifact_rr <- clean_rr
artifact_rr[c(40, 120, 121, 220)] <- c(420, 1800, 350, 1650)
long_rr <- 1000 + 35 * sin(seq(0, 90 * pi, length.out = 3600)) + rnorm(3600, sd = 15)

write.csv(data.frame(rr_ms = round(clean_rr, 3)),
          file = "inst/extdata/resting-clean-5min.csv",
          row.names = FALSE)
write.csv(data.frame(rr_ms = round(artifact_rr, 3)),
          file = "inst/extdata/resting-artifacts-5min.csv",
          row.names = FALSE)
write.csv(data.frame(rr_ms = round(long_rr, 3)),
          file = "inst/extdata/long-recording-60min.csv",
          row.names = FALSE)
