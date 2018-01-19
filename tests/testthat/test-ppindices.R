context("Test the indices")


test_that("functions handles NAs", {
  expect_equal(n_int_a(open = c(0,NA), patch = c(0,NA)), 0)
})
