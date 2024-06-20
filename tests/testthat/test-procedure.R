test_that("procedure object works", {
  procedure <- Procedure$new(npar = 5)
  expect_error(
    procedure$npar <- "bad",
    "Field `npar` is read-only"
  )
  expect_error(
    procedure$output <- "bad",
    "Field `output` is read-only"
  )
})
