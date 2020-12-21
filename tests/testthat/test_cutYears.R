# test_lNA.R
# Created on 2019-01-18
# Created by John Bullock

context("Check cutYears() output")

years    <- rep(1975:1993, each = 3)
yearFac1 <- cutYears(years, breaks = seq(1975, 1983, by = 3))
yearFac2 <- cutYears(years, breaks = seq(1975, 1990, by = 3))
yearFac1_levels <- levels(yearFac1)
yearFac2_levels <- levels(yearFac2)



test_that("cutYears() drops no data", {
  expect_identical(length(years), length(yearFac1))
  expect_identical(length(years), length(yearFac2))
})


