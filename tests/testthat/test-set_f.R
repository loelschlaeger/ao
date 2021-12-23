test_that("setting of f works", {
  expect_snapshot(set_f(f = function(x) x^2, npar = 1, lower = -1, upper = 1, check = FALSE))
  expect_error(set_f(f = function(x) x^2, npar = 0))
  expect_error(set_f(f = 1, npar = 1))
})
