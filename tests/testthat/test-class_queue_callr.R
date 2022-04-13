crew_test("initial queue", {
  x <- queue_callr$new(start = FALSE)
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
  x <- queue_callr$new(workers = 2, start = FALSE)
  on.exit(x$shutdown())
  out <- x$get_workers()
  expect_equal(nrow(out), 2)
  expect_true(out$worker[1] != out$worker[2])
  expect_equal(out$handle, list(list(), list()))
  expect_equal(out$done, rep(FALSE, 2))
  expect_equal(out$up, rep(FALSE, 2))
  expect_equal(out$free, rep(TRUE, 2))
  expect_equal(out$sent, rep(FALSE, 2))
  expect_true(all(is.na(out$task)))
  expect_equal(out$input, list(list(), list()))
})

crew_test("add task", {
  x <- queue_callr$new(start = FALSE)
  on.exit(x$shutdown())
  fun <- function(x) x
  args <- list(x = 1)
  input <- list(fun = fun, args = args)
  x$private$add_task(input = input, task = "abc")
  out <- x$get_tasks()
  expect_equal(nrow(out), 1)
  expect_true(is.character(out$task))
  expect_equal(out$input[[1]], input)
})

crew_test("push task, more workers than tasks", {
  x <- queue_callr$new(workers = 4, start = FALSE)
  fun <- function(x) x
  args <- list(x = 1)
  input <- list(fun = fun, args = args)
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
  x <- queue_callr$new(workers = 2, start = FALSE)
  input <- list(fun = function(x) x, args = list(x = 1))
  for (index in seq_len(4)) {
    x$private$add_task(input = input, task = as.character(index))
  }
  x$private$update_tasks()
  expect_equal(nrow(x$get_tasks()), 2)
  out <- x$get_workers()
  expect_false(anyNA(out$task))
})

crew_test("detect crash and shut down workers", {
  x <- queue_callr$new(workers = 2)
  on.exit(x$shutdown())
  replicate(2, x$push(fun = function() Sys.sleep(Inf)))
  crew_wait(
    fun = function() all(map_lgl(x$get_workers()$handle, ~.x$is_alive()))
  )
  x$get_workers()$handle[[1]]$kill()
  expect_error(x$crashed(), class = "crew_error")
})

crew_test("private methods to submit and update_results work", {
  x <- queue_callr$new(workers = 2, start = TRUE)
  on.exit(x$shutdown())
  on.exit(processx::supervisor_kill(), add = TRUE)
  fun <- function(x) x
  for (index in seq_len(2)) {
    input <- list(fun = fun, args = list(x = index))
    x$private$add_task(input = input, task = as.character(index))
  }
  expect_true(all(x$get_workers()$free))
  expect_false(any(x$get_workers()$sent))
  expect_false(any(x$get_workers()$done))
  x$private$update_tasks()
  expect_false(any(x$get_workers()$free))
  expect_false(any(x$get_workers()$sent))
  expect_false(any(x$get_workers()$done))
  x$private$update_workers()
  expect_true(all(x$get_workers()$sent))
  expect_false(any(x$get_workers()$free))
  expect_false(any(x$get_workers()$done))
  crew_wait(
    ~{
      x$private$update_done()
      all(x$private$workers$done)
    },
    wait = 0.1
  )
  expect_false(any(x$get_workers()$free))
  expect_true(all(x$get_workers()$sent))
  expect_true(all(x$get_workers()$done))
  expect_equal(nrow(x$get_results()), 0)
  x$private$update_results()
  expect_true(all(x$get_workers()$free))
  expect_false(any(x$get_workers()$sent))
  expect_false(any(x$get_workers()$done))
  expect_equal(nrow(x$get_results()), 2)
  for (index in seq_len(2)) {
    out <- x$pop()
    expect_false(is.null(out))
    expect_equal(out$task, as.character(out$result$result))
  }
  for (index in seq_len(2)) {
    expect_null(x$pop())
  }
  x$shutdown()
  crew_wait(
    ~!any(map_lgl(x$private$workers$handle, x$private$worker_up)),
    wait = 0.1
  )
  walk(x$get_workers()$handle, ~expect_false(.x$is_alive()))
  expect_true(all(x$get_workers()$free))
  expect_false(any(x$get_workers()$sent))
  expect_false(any(x$get_workers()$done))
})

crew_test("push and pop", {
  x <- queue_callr$new(workers = 2, start = TRUE)
  on.exit(x$shutdown())
  on.exit(processx::supervisor_kill(), add = TRUE)
  fun <- function(x) {
    Sys.sleep(0.25)
    x
  }
  for (index in seq_len(10)) {
    x$push(fun = fun, args = list(x = index))
  }
  done <- rep(FALSE, 10)
  retries <- 600
  while (!all(done) && retries > 0) {
    out <- x$pop()
    if (!is.null(out)) {
      done[out$result$result] <- TRUE
    }
    retries <- retries - 1
    Sys.sleep(0.1)
  }
  expect_true(all(done))
})
