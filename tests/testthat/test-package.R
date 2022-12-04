test_that("exceptions work", {
  expect_error(ao_stop("error"))
  expect_warning(ao_warn("warning"))
})
