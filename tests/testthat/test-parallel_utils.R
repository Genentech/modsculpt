# These tests mock the foreach/doParallel/parallel bindings to ensure that the
# user-facing helpers still manipulate parallel backends correctly without
# spawning real clusters. This prevents accidental regressions in parallel
# workflows while keeping the suite lightweight.

test_that("parallel_set configures psock backend", {
  fake_cluster <- structure(list(id = "psock"), class = "cluster")
  recorded <- new.env(parent = emptyenv())

  testthat::local_mocked_bindings(
    parallel_end = function() {
      recorded$end <- if (is.null(recorded$end)) 1L else recorded$end + 1L
    },
    .package = "modsculpt"
  )
  testthat::local_mocked_bindings(
    makePSOCKcluster = function(num_cores) {
      recorded$cores <- num_cores
      fake_cluster
    },
    .package = "parallel"
  )
  testthat::local_mocked_bindings(
    registerDoParallel = function(cl) {
      recorded$registered <- identical(cl, fake_cluster)
      invisible(NULL)
    },
    .package = "doParallel"
  )
  testthat::local_mocked_bindings(
    getDoParWorkers = function() 2,
    .package = "foreach"
  )

  expect_message(
    parallel_set(num_cores = 2, cluster_type = "psock"),
    "Using 2 cores"
  )

  expect_identical(recorded$cores, 2)
  expect_true(isTRUE(recorded$registered))
  expect_identical(recorded$end, 1L)
})

test_that("parallel_set configures fork backend", {
  fake_cluster <- structure(list(id = "fork"), class = "cluster")
  recorded <- new.env(parent = emptyenv())

  testthat::local_mocked_bindings(
    parallel_end = function() {
      recorded$end <- if (is.null(recorded$end)) 1L else recorded$end + 1L
    },
    .package = "modsculpt"
  )
  testthat::local_mocked_bindings(
    makeForkCluster = function(num_cores) {
      recorded$cores <- num_cores
      fake_cluster
    },
    .package = "parallel"
  )
  testthat::local_mocked_bindings(
    registerDoParallel = function(cl) {
      recorded$registered <- identical(cl, fake_cluster)
      invisible(NULL)
    },
    .package = "doParallel"
  )
  testthat::local_mocked_bindings(
    getDoParWorkers = function() 4,
    .package = "foreach"
  )

  expect_message(
    parallel_set(num_cores = 4, cluster_type = "fork"),
    "Using 4 cores"
  )

  expect_identical(recorded$cores, 4)
  expect_true(isTRUE(recorded$registered))
  expect_identical(recorded$end, 1L)
})

test_that("parallel_end resets to sequential when backend active", {
  reset <- new.env(parent = emptyenv())

  testthat::local_mocked_bindings(
    getDoParRegistered = function() TRUE,
    registerDoSEQ = function() {
      reset$called <- TRUE
      invisible(NULL)
    },
    .package = "foreach"
  )

  expect_silent(parallel_end())
  expect_true(isTRUE(reset$called))
})

test_that("parallel_end is no-op when backend inactive", {
  testthat::local_mocked_bindings(
    getDoParRegistered = function() FALSE,
    registerDoSEQ = function() stop("should not be called"),
    .package = "foreach"
  )

  expect_silent(parallel_end())
})

test_that("define_foreach_operand selects correct operator", {
  testthat::local_mocked_bindings(
    getDoParRegistered = function() TRUE,
    `%dopar%` = function(...) "parallel",
    `%do%` = function(...) "sequential",
    .package = "foreach"
  )
  expect_identical(define_foreach_operand(allow_par = TRUE)(), "parallel")

  testthat::local_mocked_bindings(
    getDoParRegistered = function() TRUE,
    `%dopar%` = function(...) "parallel",
    `%do%` = function(...) "sequential",
    .package = "foreach"
  )
  expect_identical(define_foreach_operand(allow_par = FALSE)(), "sequential")

  testthat::local_mocked_bindings(
    getDoParRegistered = function() FALSE,
    `%dopar%` = function(...) "parallel",
    `%do%` = function(...) "sequential",
    .package = "foreach"
  )
  expect_identical(define_foreach_operand(allow_par = TRUE)(), "sequential")
})

test_that("parallel_set validates inputs", {
  expect_error(parallel_set(num_cores = 0), ">=")
  expect_error(parallel_set(cluster_type = "invalid"), "arg")
})

test_that("parallel_set and parallel_end work with real backend", {
  skip_on_cran()
  on.exit(parallel_end(), add = TRUE)
  expect_message(parallel_set(num_cores = 1, cluster_type = "psock"), "Using 1")
  expect_true(foreach::getDoParRegistered())
  expect_equal(foreach::getDoParWorkers(), 1)
  parallel_end()
  expect_identical(foreach::getDoParName(), "doSEQ")
})
