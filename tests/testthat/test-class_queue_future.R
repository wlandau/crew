test_that("initial queue", {
  future::plan(future::sequential)
  x <- queue_future$new()
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

test_that("get workers", {
  future::plan(future::sequential)
  x <- queue_future$new(workers = 2)
  out <- x$get_workers()
  expect_equal(nrow(out), 2)
  expect_true(out$worker[1] != out$worker[2])
  expect_equal(out$handle, list(NULL, NULL))
  expect_equal(out$done, rep(FALSE, 2))
  expect_equal(out$free, rep(TRUE, 2))
  expect_equal(out$sent, rep(FALSE, 2))
  expect_equal(out$lock, rep(FALSE, 2))
  expect_equal(out$up, rep(FALSE, 2))
  expect_true(all(is.na(out$task)))
  expect_equal(out$fun, list(NULL, NULL))
  expect_equal(out$args, list(NULL, NULL))
})

test_that("get plan", {
  x <- queue_future$new(workers = 2, plan = future::sequential)
  expect_s3_class(x$get_plan(), "sequential")
})

test_that("add task", {
  future::plan(future::sequential)
  x <- queue_future$new()
  fun <- function(x) x
  args <- list(x = 1)
  x$private$add_task(fun = fun, args = args, task = "abc")
  out <- x$get_tasks()
  expect_equal(nrow(out), 1)
  expect_true(is.character(out$task))
  expect_equal(out$args[[1]], args)
})

test_that("push task, more workers than tasks", {
  future::plan(future::sequential)
  x <- queue_future$new(workers = 4)
  fun <- function(x) x
  args <- list(x = 1)
  x$private$add_task(fun = fun, args = args, task = "abc")
  x$private$add_task(fun = fun, args = args, task = "123")
  x$private$workers$free[2] <- FALSE
  x$private$send_tasks()
  expect_equal(nrow(x$get_tasks()), 0)
  out <- x$get_workers()
  expect_equal(is.na(out$task), c(FALSE, TRUE, FALSE, TRUE))
  expect_equal(out$free, c(FALSE, FALSE, FALSE, TRUE))
  expect_equal(out$fun, list(fun, NULL, fun, NULL))
})

test_that("push task, more tasks than workers", {
  future::plan(future::sequential)
  x <- queue_future$new(workers = 2)
  fun <- function(x) x
  args <- list(x = 1)
  for (index in seq_len(4)) {
    x$private$add_task(fun = fun, args = args, task = as.character(index))
  }
  x$private$send_tasks()
  expect_equal(nrow(x$get_tasks()), 2)
  out <- x$get_workers()
  expect_false(anyNA(out$task))
})

test_that("detect crash", {
  future::plan(future::sequential)
  on.exit(processx::supervisor_kill(), add = TRUE)
  x <- queue_future$new(workers = 2, plan = future.callr::callr)
  replicate(2, x$push(fun = function() Sys.sleep(Inf)))
  crew_wait(
    fun = function() all(map_lgl(x$get_workers()$handle, ~!future::resolved(.x)))
  )
  x$get_workers()$handle[[1]]$process$kill()
  expect_error(x$pop(), class = "crew_error")
  x$get_workers()$handle[[2]]$process$kill()
})

test_that("shutdown", {
  future::plan(future::sequential)
  x <- queue_future$new(
    workers = 2,
    timeout = 30,
    plan = future.callr::callr
  )
  on.exit(processx::supervisor_kill(), add = TRUE)
  fun <- function(x) {
    x
  }
  for (index in seq_len(10)) {
    x$push(fun = fun, args = list(x = index))
  }
  expect_true(all(x$get_workers()$up))
  x$shutdown(wait = TRUE)
  expect_false(any(x$get_workers()$up))
})

test_that("private methods to submit and update_results work", {
  future::plan(future::sequential)
  x <- queue_future$new(
    workers = 2,
    timeout = 30,
    plan = future.callr::callr
  )
  on.exit(processx::supervisor_kill(), add = TRUE)
  fun <- function(x) x
  for (index in seq_len(2)) {
    x$private$add_task(
      fun = fun,
      args = list(x = index),
      task = as.character(index)
    )
  }
  expect_true(all(x$get_workers()$free))
  expect_false(any(x$get_workers()$sent))
  expect_false(any(x$get_workers()$done))
  x$private$send_tasks()
  expect_false(any(x$get_workers()$free))
  expect_false(any(x$get_workers()$sent))
  expect_false(any(x$get_workers()$done))
  x$private$send_workers()
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
  x$shutdown(wait = TRUE)
  walk(x$get_workers()$handle, ~expect_true(future::resolved(.x)))
  expect_true(all(x$get_workers()$free))
  expect_false(any(x$get_workers()$sent))
  expect_false(any(x$get_workers()$done))
})

test_that("push and pop", {
  future::plan(future::sequential)
  x <- queue_future$new(
    workers = 2,
    plan = future.callr::callr,
    timeout = 30
  )
  on.exit(processx::supervisor_kill(), add = TRUE)
  fun <- function(x) {
    Sys.sleep(1)
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
