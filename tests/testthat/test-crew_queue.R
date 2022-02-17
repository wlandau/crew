test_that("initial queue", {
  x <- crew_queue$new()
  expect_true(is.data.frame(x$get_tasks()))
  expect_true(is.data.frame(x$get_workers()))
  expect_equal(nrow(x$get_tasks()), 0)
  expect_equal(nrow(x$get_workers()), 0)
})

test_that("add task", {
  x <- crew_queue$new()
  fun <- function(x) x
  args <- list(x = 1)
  tags <- letters
  x$add_task(fun = fun, args = args, tags = tags)
  out <- x$get_tasks()
  expect_equal(nrow(out), 1)
  expect_true(is.character(out$task))
  expect_equal(out$args[[1]], args)
  expect_equal(out$tags, list(letters))
})

test_that("add worker", {
  x <- crew_queue$new()
  x$add_workers(workers = 2, tags = letters)
  out <- x$get_workers()
  expect_equal(nrow(out), 2)
  expect_true(out$worker[1] != out$worker[2])
  expect_equal(out$tags, list(letters, letters))
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

test_that("remove task", {
  x <- crew_queue$new()
  fun <- function(x) x
  args <- list(x = 1)
  tags <- letters
  x$add_task(fun = fun, args = args, tags = tags)
  x$add_task(fun = fun, args = args, tags = tags)
  out <- x$get_tasks()
  expect_equal(nrow(out), 2)
  expect_true(out$task[1] != out$task[2])
  x$remove_task("nope")
  out2 <- x$get_tasks()
  expect_equal(out, out2)
  x$remove_task(out$task[1])
  expect_equal(x$get_tasks(), out[2, ])
})

test_that("remove worker", {
  x <- crew_queue$new()
  x$add_workers(workers = 2)
  out <- x$get_workers()
  expect_equal(nrow(out), 2)
  expect_true(out$worker[1] != out$worker[2])
  x$remove_worker("nope")
  out2 <- x$get_workers()
  expect_equal(out, out2)
  x$remove_worker(out$worker[1])
  expect_equal(x$get_workers(), out[2, ])
})
