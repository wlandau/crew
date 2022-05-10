crew_test("no work then worker timeout", {
  store <- crew_store_local$new()
  expect_error(
    crew_worker(
      worker = "name",
      store = store$marshal(),
      max_tasks = Inf,
      timeout = 0,
      wait = 0
    ),
    class = "crew_expire"
  )
})

crew_test("one simple task", {
  root <- tempfile()
  dir_create(root)
  store <- crew_store_local$new(root = root)
  fun <- function(x) {
    x + 1
  }
  value <- structure(
    list(fun = deparse(fun), args = list(x = 1L)),
    class = "crew_task"
  )
  store$write_input(worker = "worker", value = value)
  expect_error(
    crew_worker(
      worker = "worker",
      store = store$marshal(),
      max_tasks = Inf,
      timeout = 0,
      wait = 0
    ),
    class = "crew_expire"
  )
  expect_false(store$exists_input("worker"))
  expect_true(store$exists_output("worker"))
  task <- store$read_output("worker")
  expect_equal(task$result, 2L)
})

crew_test("one task", {
  root <- tempfile()
  dir_create(root)
  store <- crew_store_local$new(root = root)
  fun <- function(x) {
    x + 1
  }
  value <- structure(
    list(fun = deparse(fun), args = list(x = 1L)),
    class = "crew_task"
  )
  store$write_input(worker = "worker", value = value)
  crew_worker(
    worker = "worker",
    store = store$marshal(),
    max_tasks = 1,
    timeout = 0,
    wait = 0
  )
  expect_false(store$exists_input("worker"))
  expect_true(store$exists_output("worker"))
  task <- store$read_output("worker")
  expect_equal(task$result, 2L)
})

crew_test("crew_monad() without errors", {
  out <- crew_monad(fun = identity, args = list(x = "x"))
  expect_equal(out$result, "x")
  expect_true(is.numeric(out$seconds) && length(out$seconds) == 1)
  expect_null(out$error)
  expect_null(out$warnings)
  expect_null(out$traceback)
})

crew_test("crew_monad() with errors", {
  fun <- function() {
    warning("warning1")
    warning("warning2")
    stop("error1")
  }
  suppressWarnings(out <- crew_monad(fun = fun, args = list()))
  expect_null(out$result)
  expect_true(is.numeric(out$seconds))
  expect_equal(length(out$seconds), 1)
  expect_equal(out$error, "error1")
  expect_true(is.character(out$traceback) && length(out$traceback) > 0)
  expect_equal(out$warnings, c("warning1", "warning2"))
})
