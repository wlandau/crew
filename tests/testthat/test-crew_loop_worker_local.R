test_that("no work then worker timeout", {
  expect_error(
    crew_loop_worker_local(
      name = "name",
      dir_root = fs::dir_create(tempfile()),
      timeout = 0,
      wait_input = 0
    ),
    class = "crew_timeout"
  )
})

test_that("one job that runs and shutdowns the worker", {
  dir_root <- fs::dir_create(tempfile())
  store <- class_store_local$new(dir_root = dir_root)
  fun <- function(x, dir_root) {
    store <- class_store_local$new(dir_root = dir_root)
    fun <- function() rlang::abort(class = "crew_shutdown")
    data <- list(fun = deparse(fun), args = list())
    store$write_input(name = "worker", data = data)
    x + 1L
  }
  args <- list(x = 0L, dir_root = dir_root)
  data <- list(fun = deparse(fun), args = args)
  store$write_input(name = "worker", data = data)
  crew_loop_worker_local(
    name = "worker",
    dir_root = dir_root,
    timeout = Inf,
    wait_input = 0
  )
  expect_false(store$exists_input("worker"))
  expect_true(store$exists_output("worker"))
  expect_equal(store$read_output("worker"), 1L)
})
