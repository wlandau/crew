test_that("initial queue", {
  x <- crew_queue$new()
  expect_true(is.data.frame(x$get_tasks()))
  expect_true(is.data.frame(x$get_workers()))
  expect_equal(nrow(x$get_tasks()), 0)
  expect_equal(nrow(x$get_workers()), 0)
})

test_that("push", {
  x <- crew_queue$new()
  fun <- function(x) x
  args <- list(x = 1)
  x$push(fun = fun, args = args)
  out <- x$get_tasks()
  expect_equal(nrow(out), 1)
  expect_true(is.character(out$task))
  expect_equal(out$args[[1]], args)
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
  expect_equal(out$lock, rep(FALSE, 2))
  expect_true(all(is.na(out$task)))
  expect_equal(out$fun, list(NULL, NULL))
  expect_equal(out$args, list(NULL, NULL))
  expect_equal(out$result, list(NULL, NULL))
})

test_that("assign tasks, more workers than tasks", {
  x <- crew_queue$new()
  fun <- function(x) x
  args <- list(x = 1)
  x$push(fun = fun, args = args)
  x$push(fun = fun, args = args)
  x$add_workers(workers = 4)
  x$private$workers$free[2] <- FALSE
  x$private$assign_tasks()
  expect_equal(nrow(x$get_tasks()), 0)
  out <- x$get_workers()
  expect_equal(is.na(out$task), c(FALSE, TRUE, FALSE, TRUE))
  expect_equal(out$free, c(FALSE, FALSE, FALSE, TRUE))
  expect_equal(out$fun, list(fun, NULL, fun, NULL))
})

test_that("assign tasks, more tasks than workers", {
  x <- crew_queue$new()
  fun <- function(x) x
  args <- list(x = 1)
  for (index in seq_len(4)) {
    x$push(fun = fun, args = args)
  }
  x$add_workers(workers = 2)
  x$private$assign_tasks()
  expect_equal(nrow(x$get_tasks()), 2)
  out <- x$get_workers()
  expect_false(anyNA(out$task))
})
