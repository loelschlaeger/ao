test_that("ao works", {
  himmelblau <- function(x) (x[1]^2 + x[2] - 11)^2 + (x[1] + x[2]^2 - 7)^2
  out <- ao(f = himmelblau, p = c(0,0), partition = list(1, 2),
     optimizer = set_optimizer_optim(lower = -5, upper = 5, method = "L-BFGS-B"))
  expect_type(out, "list")
  expect_named(out, c("optimum", "estimate", "sequence", "time"))
})
