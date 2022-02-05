test_that("no work then worker timeout", {
  store <- class_store_local$new()
  expect_error(
    crew_worker_loop(
      name = "name",
      store = store,
      timeout = 0,
      wait_input = 0
    ),
    class = "crew_timeout"
  )
})

test_that("one job that runs and shutdowns the worker", {
  dir_root <- fs::dir_create(tempfile())
  store <- class_store_local$new(dir_root = dir_root)
  fun <- function(x, store) {
    fun <- function() rlang::abort(class = "crew_shutdown")
    data <- list(fun = deparse(fun), args = list())
    store$write_input(name = "worker", data = data)
    x + 1L
  }
  args <- list(x = 0L, store = store)
  data <- list(fun = deparse(fun), args = args)
  store$write_input(name = "worker", data = data)
  crew_worker_loop(
    name = "worker",
    store = store,
    timeout = Inf,
    wait_input = 0
  )
  expect_false(store$exists_input("worker"))
  expect_true(store$exists_output("worker"))
  expect_equal(store$read_output("worker"), 1L)
})
