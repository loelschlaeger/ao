test_that("ao works",{
    f <- set_f(f = function(x) 3*x[1]^2 + 2*x[1]*x[2] + x[2]^2 - 5*x[1] + 2,
               npar = 2, check = FALSE)
    expect_s3_class(ao(f = f, partition = list(1, 2), iterations = 2), "ao")
  }
)
