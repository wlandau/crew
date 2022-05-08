crew_test("detect crash and shut down workers", {
  x <- crew_queue_session$new(workers = 2)
  on.exit(x$shutdown())
  on.exit(processx::supervisor_kill(), add = TRUE)
  replicate(2, x$push(fun = function() Sys.sleep(Inf)))
  crew_wait(
    fun = function() all(map_lgl(x$get_workers()$handle, ~.x$is_alive()))
  )
  x$get_workers()$handle[[1]]$kill()
  expect_error(x$crashed(), class = "crew_error")
})

crew_test("private methods to submit and update_results work", {
  x <- crew_queue_session$new(workers = 2)
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

crew_test("available", {
  x <- crew_queue_session$new(workers = 2)
  on.exit(x$shutdown())
  on.exit(processx::supervisor_kill(), add = TRUE)
  fun <- function() "x"
  x$push(fun = fun, update = FALSE)
  expect_false(x$private$available())
})

crew_test("update", {
  x <- crew_queue_session$new(workers = 2)
  on.exit(x$shutdown())
  fun <- function() "x"
  x$push(fun = fun, update = FALSE)
  expect_null(x$pop(update = FALSE))
  retries <- 600
  result <- NULL
  while (is.null(result) && retries > 0) {
    x$update()
    result <- x$pop(update = FALSE)$result$result
    retries <- retries - 1
    Sys.sleep(0.1)
  }
  expect_equal(result, "x")
})

crew_test("push and pop", {
  x <- crew_queue_session$new(workers = 2)
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
