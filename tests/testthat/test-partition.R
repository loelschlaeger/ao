test_that("Partition object can be created", {
  npar <- 5
  partition <- Partition$new(npar = npar, type = "random")
  checkmate::expect_list(partition$get("random"))
  expect_error(partition$get("fixed"), "not yet defined")
  expect_error(
    partition$define_fixed_partition(list(1, 2:3, 3:6)),
    "must only contain"
  )
  partition$define_fixed_partition(list(1, 2:3, 3:5))
  checkmate::expect_list(partition$get("fixed"))
  expect_equal(partition$get("sequential"), as.list(1:npar))
  expect_equal(partition$get("none"), list(1:npar))
})


