# This file is part of the standard setup for testthat.
# It is recommended that you do not modify it.
#
# Where should you do additional test configuration?
# Learn more about the roles of various files in:
# * https://r-pkgs.org/testing-design.html#sec-tests-files-overview
# * https://testthat.r-lib.org/articles/special-files.html

library(testthat)
library(mercure.ulg)
testthat::test_that("from03602180 swaps halves correctly", {
  mat <- matrix(1:8, nrow = 2)
  result <- from03602180(mat)
  expected <- matrix(c(5, 6, 7, 8, 1, 2, 3, 4), nrow = 2)
  testthat::expect_equal(result, expected)
})




