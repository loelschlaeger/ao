test_that("euclidean works", {
  x <- 1:10
  y <- 1:10
  expect_equal(euclidean(x, y), 0)
})
