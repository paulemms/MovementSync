test_that("The start and end of a time series are identified", {
  dfr <- data.frame(a = c(NA, 1, 2, NA), b = c(NA, NA, NA, NA), c = 1:4, d = c(1:3, NA))
  se_dfr <- get_start_end(dfr)
  expected_dfr <- data.frame(Start = c(2, NA, 1, 1), End = c(3, NA, 4, 3))
  rownames(expected_dfr) <- colnames(dfr)

  expect_equal(se_dfr, expected_dfr)
})

