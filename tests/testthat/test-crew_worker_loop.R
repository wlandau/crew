test_that("no work then worker timeout", {
  store <- class_store_local$new()
  expect_error(
    crew_worker_loop(
      name = "name",
      store = store$marshal(),
      timeout = 0,
      wait = 0
    ),
    class = "crew_timeout"
  )
})

test_that("one simple job", {
  dir_root <- tempfile()
  dir_create(dir_root)
  store <- class_store_local$new(dir_root = dir_root)
  fun <- function(x) {
    x + 1
  }
  data <- list(fun = deparse(fun), args = list(x = 1L))
  store$write_input(name = "worker", data = data)
  expect_error(
    crew_worker_loop(
      name = "worker",
      store = store$marshal(),
      timeout = 0,
      wait = 0
    ),
    class = "crew_timeout"
  )
  expect_false(store$exists_input("worker"))
  expect_true(store$exists_output("worker"))
  expect_equal(store$read_output("worker")$value, 2L)
})

test_that("one job that runs and shutdowns the worker", {
  dir_root <- tempfile()
  dir_create(dir_root)
  store <- class_store_local$new(dir_root = dir_root)
  fun <- function() {
    rlang::abort(message = "x", class = "crew_shutdown")
  }
  data <- list(fun = deparse(fun), args = list())
  store$write_input(name = "worker", data = data)
  crew_worker_loop(
    name = "worker",
    store = store$marshal(),
    timeout = 1,
    wait = 0
  )
  expect_true(store$exists_input("worker"))
  expect_false(store$exists_output("worker"))
})

test_that("crew_worker_loop_monad() without errors", {
  out <- crew_worker_loop_monad(fun = identity, args = list(x = "x"))
  expect_equal(out$value, "x")
  expect_true(is.numeric(out$seconds) && length(out$seconds) == 1)
  expect_null(out$error)
  expect_null(out$warnings)
  expect_null(out$traceback)
})


test_that("crew_worker_loop_monad() with errors", {
  fun <- function() {
    warning("warning1")
    warning("warning2")
    stop("error1")
  }
  suppressWarnings(out <- crew_worker_loop_monad(fun = fun, args = list()))
  expect_null(out$value)
  expect_true(is.numeric(out$seconds))
  expect_equal(length(out$seconds), 1)
  expect_equal(out$error, "error1")
  expect_true(is.character(out$traceback) && length(out$traceback) > 0)
  expect_equal(out$warnings, c("warning1", "warning2"))
})

test_that("crew_worker_loop_monad() with worker shutdown", {
  fun <- function() {
    crew_shutdown()
  }
  expect_error(
    crew_worker_loop_monad(fun = fun, args = list()),
    class = "crew_shutdown"
  )
})
