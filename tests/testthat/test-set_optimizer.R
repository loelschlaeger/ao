test_that("setting of optimizer works", {
  expect_snapshot(
    set_optimizer(
      f = set_f(f = function(x) x^2, npar = 1), optimizer = stats::optim,
      f_arg = "fn", p_arg = "par", lower = -2, upper = -1
    )
  )
})
