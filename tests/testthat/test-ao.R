test_that(
  "test input checks",
  {
    f <- function(x) 3 * x[1]^2 + 2 * x[1] * x[2] + x[2]^2 - 5 * x[1] + 2
    npar <- 2
    sequence <- rep(c(1, 2), 10)
    groups <- list(1, 2)
    expect_error(
      ao(f = NULL, npar = npar, sequence = sequence, groups = groups)
    )
    expect_error(
      ao(f = f, npar = 0, sequence = sequence, groups = groups)
    )
    expect_error(
      ao(f = f, npar = npar, sequence = 1:3, groups = groups)
    )
    expect_error(
      ao(f = f, npar = npar, sequence = 1:3, groups = 1)
    )
    expect_error(
      ao(f = f, npar = npar, sequence = 1:3, groups = list(1, 2, 3))
    )
  }
)

test_that(
  "test output",
  {
    f <- function(x) 3 * x[1]^2 + 2 * x[1] * x[2] + x[2]^2 - 5 * x[1] + 2
    npar <- 2
    sequence <- rep(c(1, 2), 10)
    groups <- list(1, 2)
    out <- ao(f = f, npar = npar, sequence = sequence, groups = groups)
    expect_s3_class(
      ao(f = f, npar = npar, sequence = sequence, groups = groups),
      "ao"
    )
  }
)
