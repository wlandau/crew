crew_test("no work then worker timeout", {
  store <- crew_store_local$new()
  expect_error(
    crew_worker_loop(
      worker = "name",
      store = store$marshal(),
      timeout = 0,
      wait = 0
    ),
    class = "crew_timeout"
  )
})

crew_test("one simple job", {
  dir_root <- tempfile()
  dir_create(dir_root)
  store <- crew_store_local$new(dir_root = dir_root)
  fun <- function(x) {
    x + 1
  }
  value <- structure(
    list(fun = deparse(fun), args = list(x = 1L)),
    class = "crew_task"
  )
  store$write_worker_input(worker = "worker", value = value)
  expect_error(
    crew_worker_loop(
      worker = "worker",
      store = store$marshal(),
      timeout = 0,
      wait = 0
    ),
    class = "crew_timeout"
  )
  expect_false(store$exists_worker_input("worker"))
  expect_true(store$exists_worker_output("worker"))
  job <- store$read_worker_output("worker")
  expect_s3_class(job, "crew_task")
  expect_equal(job$result, 2L)
})

crew_test("shutdown job", {
  dir_root <- tempfile()
  dir_create(dir_root)
  store <- crew_store_local$new(dir_root = dir_root)
  fun <- function() FALSE
  value <- structure(
    list(fun = deparse(fun), args = list()),
    class = "crew_shutdown"
  )
  store$write_worker_input(worker = "worker", value = value)
  crew_worker_loop(
    worker = "worker",
    store = store$marshal(),
    timeout = 1,
    wait = 0
  )
  expect_false(store$exists_worker_input("worker"))
  expect_true(store$exists_worker_output("worker"))
  expect_s3_class(store$read_worker_output("worker"), "crew_shutdown")
})

crew_test("crew_worker_loop_monad() without errors", {
  out <- crew_worker_loop_monad(fun = identity, args = list(x = "x"))
  expect_equal(out$result, "x")
  expect_true(is.numeric(out$seconds) && length(out$seconds) == 1)
  expect_null(out$error)
  expect_null(out$warnings)
  expect_null(out$traceback)
})

crew_test("crew_worker_loop_monad() with errors", {
  fun <- function() {
    warning("warning1")
    warning("warning2")
    stop("error1")
  }
  suppressWarnings(out <- crew_worker_loop_monad(fun = fun, args = list()))
  expect_null(out$result)
  expect_true(is.numeric(out$seconds))
  expect_equal(length(out$seconds), 1)
  expect_equal(out$error, "error1")
  expect_true(is.character(out$traceback) && length(out$traceback) > 0)
  expect_equal(out$warnings, c("warning1", "warning2"))
})
