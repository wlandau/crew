test_that("initial queue", {
  x <- crew_queue$new()
  expect_true(is.data.frame(x$get_tasks()))
  expect_true(is.data.frame(x$get_results()))
  expect_true(is.data.frame(x$get_workers()))
  expect_equal(nrow(x$get_tasks()), 0)
  expect_equal(nrow(x$get_results()), 0)
  expect_equal(nrow(x$get_workers()), 0)
  expect_gt(ncol(x$get_tasks()), 0)
  expect_gt(ncol(x$get_results()), 0)
  expect_gt(ncol(x$get_workers()), 0)
})

test_that("add workers", {
  x <- crew_queue$new()
  x$add_workers(workers = 2)
  out <- x$get_workers()
  expect_equal(nrow(out), 2)
  expect_true(out$worker[1] != out$worker[2])
  expect_equal(out$handle, list(NULL, NULL))
  expect_equal(out$up, rep(FALSE, 2))
  expect_equal(out$done, rep(FALSE, 2))
  expect_equal(out$free, rep(TRUE, 2))
  expect_equal(out$sent, rep(FALSE, 2))
  expect_true(all(is.na(out$task)))
  expect_equal(out$fun, list(NULL, NULL))
  expect_equal(out$args, list(NULL, NULL))
})

test_that("remove workers", {
  x <- crew_queue$new()
  on.exit(x$shutdown())
  x$add_workers(workers = 4)
  grid <- expand.grid(
    free = c(TRUE, FALSE),
    up = c(TRUE, FALSE)
  )
  for (field in colnames(grid)) {
    x$private$workers[[field]] <- grid[[field]]
  }
  for (index in seq_len(4)) {
    if (grid$up[index]) {
      x$private$workers$handle[[index]] <- callr::r_session$new(wait = TRUE)
    }
  }
  x$remove_workers()
  workers <- x$get_workers()
  expect_equal(nrow(x$get_workers()), 3)
  expect_true(all(!workers$free | workers$up))
})

test_that("add task", {
  x <- crew_queue$new()
  fun <- function(x) x
  args <- list(x = 1)
  x$private$add_task(fun = fun, args = args, task = "abc")
  out <- x$get_tasks()
  expect_equal(nrow(out), 1)
  expect_true(is.character(out$task))
  expect_equal(out$args[[1]], args)
})

test_that("push task, more workers than tasks", {
  x <- crew_queue$new()
  fun <- function(x) x
  args <- list(x = 1)
  x$private$add_task(fun = fun, args = args, task = "abc")
  x$private$add_task(fun = fun, args = args, task = "123")
  x$add_workers(workers = 4)
  x$private$workers$free[2] <- FALSE
  x$private$assign_tasks()
  expect_equal(nrow(x$get_tasks()), 0)
  out <- x$get_workers()
  expect_equal(is.na(out$task), c(FALSE, TRUE, FALSE, TRUE))
  expect_equal(out$free, c(FALSE, FALSE, FALSE, TRUE))
  expect_equal(out$fun, list(fun, NULL, fun, NULL))
})

test_that("push task, more tasks than workers", {
  x <- crew_queue$new()
  fun <- function(x) x
  args <- list(x = 1)
  for (index in seq_len(4)) {
    x$private$add_task(fun = fun, args = args, task = as.character(index))
  }
  x$add_workers(workers = 2)
  x$private$assign_tasks()
  expect_equal(nrow(x$get_tasks()), 2)
  out <- x$get_workers()
  expect_false(anyNA(out$task))
})

test_that("private methods to submit and receive_results work", {
  x <- crew_queue$new()
  on.exit(x$shutdown())
  fun <- function(x) x
  for (index in seq_len(2)) {
    x$private$add_task(
      fun = fun,
      args = list(x = index),
      task = as.character(index)
    )
  }
  x$add_workers(workers = 2)
  expect_false(any(x$get_workers()$up))
  expect_true(all(x$get_workers()$free))
  expect_false(any(x$get_workers()$sent))
  expect_false(any(x$get_workers()$done))
  x$private$assign_tasks()
  expect_false(any(x$get_workers()$up))
  expect_false(any(x$get_workers()$free))
  expect_false(any(x$get_workers()$sent))
  expect_false(any(x$get_workers()$done))
  x$private$send_tasks()
  expect_true(all(x$get_workers()$sent))
  while (!all(x$private$workers$up)) {
    x$private$poll_up()
    Sys.sleep(0.1)
  }
  expect_true(all(x$get_workers()$up))
  expect_false(any(x$get_workers()$free))
  expect_true(all(x$get_workers()$sent))
  expect_false(any(x$get_workers()$done))
  while (!all(x$private$workers$done)) {
    x$private$poll_done()
    Sys.sleep(0.1)
  }
  expect_true(all(x$get_workers()$up))
  expect_false(any(x$get_workers()$free))
  expect_true(all(x$get_workers()$sent))
  expect_true(all(x$get_workers()$done))
  expect_equal(nrow(x$get_results()), 0)
  x$private$receive_results()
  expect_true(all(x$get_workers()$up))
  expect_true(all(x$get_workers()$free))
  expect_false(any(x$get_workers()$sent))
  expect_false(any(x$get_workers()$done))
  expect_equal(nrow(x$get_results()), 2)
  for (index in seq_len(2)) {
    out <- x$private$pop_result()
    expect_false(is.null(out))
    expect_equal(out$task, as.character(out$result$result))
  }
  for (index in seq_len(2)) {
    expect_null(x$private$pop_result())
  }
  x$shutdown()
  while (any(x$private$workers$up)) {
    x$private$poll_up()
    Sys.sleep(0.1)
  }
  expect_false(any(x$get_workers()$up))
  walk(x$get_workers()$handle, ~expect_false(.x$is_alive()))
})

test_that("push and pop", {
  x <- crew_queue$new()
  on.exit(x$shutdown())
  fun <- function(x) x
})

