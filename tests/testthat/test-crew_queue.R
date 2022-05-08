crew_test("crew_queue_worker_start()", {
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
  crew_queue_worker_start(
    worker = "worker",
    store = store$marshal(),
    jobs = 1,
    timeout = 0,
    wait = 0
  )
  expect_false(store$exists_worker_input("worker"))
  expect_true(store$exists_worker_output("worker"))
  job <- store$read_worker_output("worker")
  expect_equal(job$result, 2L)
})

crew_test("initial queue", {
  x <- crew_queue$new()
  expect_true(is.data.frame(x$get_tasks()))
  expect_true(is.data.frame(x$get_results()))
  expect_true(is.data.frame(x$get_workers()))
  expect_equal(nrow(x$get_tasks()), 0)
  expect_equal(nrow(x$get_results()), 0)
  expect_equal(nrow(x$get_workers()), 1)
  expect_gt(ncol(x$get_tasks()), 0)
  expect_gt(ncol(x$get_results()), 0)
  expect_gt(ncol(x$get_workers()), 1)
})

crew_test("get workers", {
  x <- crew_queue$new(workers = 2)
  on.exit(callr_force_shutdown(x))
  out <- x$get_workers()
  expect_equal(nrow(out), 2)
  expect_true(out$worker[1] != out$worker[2])
  expect_equal(out$handle, list(list(), list()))
  expect_equal(out$up, rep(FALSE, 2))
  expect_equal(out$done, rep(FALSE, 2))
  expect_equal(out$free, rep(TRUE, 2))
  expect_equal(out$sent, rep(FALSE, 2))
  expect_true(all(is.na(out$task)))
  expect_equal(out$input, list(list(), list()))
})

crew_test("add task", {
  x <- crew_queue$new()
  input <- list(fun = function(x) x, args = list(x = 1))
  x$private$add_task(input = input, task = "abc")
  out <- x$get_tasks()
  expect_equal(nrow(out), 1)
  expect_true(is.character(out$task))
  expect_equal(out$input[[1]], input)
})

crew_test("push task, more workers than tasks", {
  x <- crew_queue$new(workers = 4)
  input <- list(fun = function(x) x, args = list(x = 1))
  x$private$add_task(input = input, task = "abc")
  x$private$add_task(input = input, task = "123")
  x$private$workers$free[2] <- FALSE
  x$private$update_tasks()
  expect_equal(nrow(x$get_tasks()), 0)
  out <- x$get_workers()
  expect_equal(is.na(out$task), c(FALSE, TRUE, FALSE, TRUE))
  expect_equal(out$free, c(FALSE, FALSE, FALSE, TRUE))
  expect_equal(out$input, list(input, list(), input, list()))
})

crew_test("push task, more tasks than workers", {
  x <- crew_queue$new(workers = 2)
  input <- list(fun = function(x) x, args = list(x = 1))
  for (index in seq_len(4)) {
    x$private$add_task(input = input, task = as.character(index))
  }
  x$private$update_tasks()
  expect_equal(nrow(x$get_tasks()), 2)
  out <- x$get_workers()
  expect_false(anyNA(out$task))
})
