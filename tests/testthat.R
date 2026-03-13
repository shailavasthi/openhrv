if (!requireNamespace("testthat", quietly = TRUE)) {
  quit(save = "no", status = 0)
}

library(testthat)
library(openhrv)

test_check("openhrv")
