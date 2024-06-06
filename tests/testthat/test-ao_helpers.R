test_that("block objective function can be built", {
  partition <- Partition$new(npar = 2, type = "sequential")
  objective <- Objective$new(
    f = function(x) {
      value <- (x[1]^2 + x[2] - 11)^2 + (x[1] + x[2]^2 - 7)^2
      gradient <- c(
        4 * x[1] * (x[1]^2 + x[2] - 11) + 2 * (x[1] + x[2]^2 - 7),
        2 * (x[1]^2 + x[2] - 11) + 4 * x[2] * (x[1] + x[2]^2 - 7)
      )
      hessian <- matrix(
        c(
          12 * x[1]^2 + 4 * x[2] - 42,
          4 * (x[1] + x[2]),
          4 * (x[1] + x[2]),
          4 * x[1] + 12 * x[2]^2 - 26
        ),
        nrow = 2, ncol = 2
      )
      structure(value, "gradient" = gradient, "hessian" = hessian)
    },
    npar = 2
  )
  block_objective <- ao_build_block_objective(partition)
  expect_equal(block_objective(3, 2, 1), 0)
  partition$define_block_attribute("gradient", function(x, y) x[y])
  expect_equal(block_objective(3, 2, 1), structure(0, gradient = 0))
  partition$define_block_attribute("hessian", function(x, y) x[y, y, drop = FALSE])
  expect_equal(
    block_objective(3, 2, 1),
    structure(0, gradient = 0, hessian = structure(74, dim = c(1L, 1L)))
  )
  partition$define_block_attribute("does_not_exist", function(x, y) 1)
  expect_equal(
    block_objective(3, 2, 1),
    structure(
      0,
      gradient = 0, hessian = structure(74, dim = c(1L, 1L)),
      does_not_exist = 1
    )
  )
})
