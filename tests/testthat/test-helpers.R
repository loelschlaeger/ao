test_that("estimate can be split by target", {
  estimate <- 1:5
  target <- c("a", "b", "c")
  npar <- c(2, 2, 1)
  split <- split_by_target(estimate, target, npar)
  checkmate::expect_list(split, types = "integer", len = length(npar))
  expect_equal(split_by_target(estimate, NULL, npar), NULL)
})

test_that("random partition can be generated", {
  min <- 2
  partition <- generate_random_partition(1:5, 0.5, min)
  checkmate::expect_list(partition, types = "integer", min.len = min)
  expect_equal(sort(unlist(partition)), 1:5)
  expect_equal(generate_random_partition(1:5, 0.5, 5), as.list(1:5))
})

test_that("random partition generation is deterministic with a seed", {
  set.seed(123)
  partition_1 <- generate_random_partition(1:10, 0.5, 2)
  set.seed(123)
  partition_2 <- generate_random_partition(1:10, 0.5, 2)
  expect_identical(partition_1, partition_2)
})

test_that("results of multiple processes can be merged", {
  results <- list(
    list(
      estimate = c(1, 2), estimate_split = list(a = 1, b = 2), value = 3,
      details = data.frame(iteration = 0:1, value = c(5, 3)), seconds = 0.5,
      stopping_reason = "first"
    ),
    list(
      estimate = c(3, 4), estimate_split = list(a = 3, b = 4), value = 2,
      details = data.frame(iteration = 0, value = 2), seconds = 0.25,
      stopping_reason = "second"
    )
  )
  processes <- data.frame(process = 1:2)
  merged <- merge_results(results, minimize = TRUE, processes = processes)
  expect_named(
    merged,
    c(
      "estimate", "estimates", "estimate_split", "value", "values", "details",
      "seconds", "seconds_each", "stopping_reason", "stopping_reasons",
      "processes"
    )
  )
  expect_identical(merged$estimate, c(3, 4))
  expect_identical(merged$estimate_split, list(a = 3, b = 4))
  expect_identical(merged$value, 2)
  expect_identical(merged$values, list(3, 2))
  expect_identical(merged$details$process, c(1L, 1L, 2L))
  expect_identical(merged$seconds, 0.75)
  expect_identical(merged$stopping_reason, "second")
  expect_identical(merged$processes, processes)
  merged <- merge_results(results, minimize = FALSE, add_details = FALSE)
  expect_named(
    merged,
    c("estimate", "estimate_split", "value", "seconds", "stopping_reason")
  )
  expect_identical(merged$estimate, c(1, 2))
  expect_identical(merged$stopping_reason, "first")
})

test_that("best process is selected when merging results", {
  results <- list(
    list(estimate = 1, value = 3, seconds = 0, stopping_reason = "first"),
    list(estimate = 2, value = 1, seconds = 0, stopping_reason = "second"),
    list(estimate = 3, value = 2, seconds = 0, stopping_reason = "third")
  )
  expect_identical(merge_results(results, add_details = FALSE)$estimate, 2)
  expect_identical(
    merge_results(results, minimize = FALSE, add_details = FALSE)$estimate, 1
  )
  results[[1]]$value <- NA_real_
  results[[2]]$value <- 3
  results[[3]]$value <- 1
  expect_identical(merge_results(results, add_details = FALSE)$estimate, 3)
  results[[2]]$value <- NaN
  results[[3]]$value <- NA_real_
  expect_identical(
    merge_results(results, minimize = FALSE, add_details = FALSE)$estimate, 1
  )
})
