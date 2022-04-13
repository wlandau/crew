crew_test("queue_future_worker_start()", {
  fun <- function(x) x
  args <- list(x = "x")
  value <- list(fun = deparse(fun), args = args)
  store <- store_local$new()
  store$write_worker_input(worker = "worker", value = value)
  out <- queue_future_worker_start(
    worker = "worker",
    store = store$marshal(),
    timeout = 30,
    wait = 0.01,
    plan = future::sequential,
    task = "task"
  )
  expect_equal(out$resolved, FALSE)
  future <- out$future
  expect_null(future::value(future))
  expect_equal(store$read_worker_output("worker")$result, "x")
})

crew_test("queue_future_worker_start()", {
  future::plan(future::sequential)
  future <- future::future("x")
  expect_true(queue_future_worker_resolve(future)$resolved)
})

crew_test("initial queue", {
  future::plan(future::sequential)
  x <- queue_future$new()
  on.exit(x$shutdown())
  on.exit(processx::supervisor_kill(), add = TRUE)
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
  future::plan(future::sequential)
  x <- queue_future$new(workers = 2)
  on.exit(x$shutdown())
  on.exit(processx::supervisor_kill(), add = TRUE)
  out <- x$get_workers()
  expect_equal(nrow(out), 2)
  expect_true(out$worker[1] != out$worker[2])
  expect_equal(out$handle, list(list(), list()))
  expect_equal(out$up, rep(FALSE, 2))
  expect_equal(out$done, rep(FALSE, 2))
  expect_equal(out$free, rep(TRUE, 2))
  expect_equal(out$sent, rep(FALSE, 2))
  expect_equal(out$lock, rep(FALSE, 2))
  expect_true(all(is.na(out$task)))
  expect_equal(out$input, list(list(), list()))
})

crew_test("get plan", {
  x <- queue_future$new(workers = 2, plan = future::sequential)
  expect_s3_class(x$get_plan(), "sequential")
})

crew_test("add task", {
  future::plan(future::sequential)
  x <- queue_future$new()
  on.exit(x$shutdown())
  on.exit(processx::supervisor_kill(), add = TRUE)
  input <- list(fun = function(x) x, args = list(x = 1))
  x$private$add_task(input = input, task = "abc")
  out <- x$get_tasks()
  expect_equal(nrow(out), 1)
  expect_true(is.character(out$task))
  expect_equal(out$input[[1]], input)
})

crew_test("push task, more workers than tasks", {
  future::plan(future::sequential)
  x <- queue_future$new(workers = 4)
  on.exit(x$shutdown())
  on.exit(processx::supervisor_kill(), add = TRUE)
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
  future::plan(future::sequential)
  x <- queue_future$new(workers = 2)
  on.exit(x$shutdown())
  on.exit(processx::supervisor_kill(), add = TRUE)
  input <- list(fun = function(x) x, args = list(x = 1))
  for (index in seq_len(4)) {
    x$private$add_task(input = input, task = as.character(index))
  }
  x$private$update_tasks()
  expect_equal(nrow(x$get_tasks()), 2)
  out <- x$get_workers()
  expect_false(anyNA(out$task))
})

crew_test("detect crash", {
  future::plan(future::sequential)
  x <- queue_future$new(
    workers = 1,
    processes = 1,
    plan = future::sequential
  )
  on.exit(x$shutdown())
  on.exit(processx::supervisor_kill(), add = TRUE)
  x$push(fun = function() Sys.sleep(Inf))
  crew_wait(
    fun = function() {
      all(x$get_workers()$sent) &&
        !any(x$get_workers()$free)
        !any(x$private$subqueue$get_workers()$free)
    }
  )
  x$private$subqueue$get_workers()$handle[[1]]$kill()
  x$private$workers$handle[[1]]$resolved <- TRUE
  expect_error(x$crashed(), class = "crew_error")
})

crew_test("worker up", {
  future::plan(future::sequential)
  x <- queue_future$new(
    workers = 2,
    processes = 1,
    plan = future::sequential
  )
  on.exit(x$shutdown())
  on.exit(processx::supervisor_kill(), add = TRUE)
  expect_false(x$private$worker_up(list()))
  expect_false(x$private$worker_up(list(future = "x", resolved = TRUE)))
  handle <- list(future = future::future("x"), resolved = FALSE)
  expect_true(x$private$worker_up(handle, "abc"))
})

crew_test("private methods to submit and update_results work", {
  future::plan(future::sequential)
  x <- queue_future$new(
    workers = 2,
    processes = 2,
    timeout = 30,
    plan = future::sequential
  )
  on.exit(x$shutdown())
  on.exit(processx::supervisor_kill(), add = TRUE)
  
  for (index in seq_len(2)) {
    input = list(fun = function(x) x, args = list(x = index))
    x$private$add_task(
      input = input,
      task = as.character(index)
    )
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
})

crew_test("push and pop", {
  future::plan(future::sequential)
  x <- queue_future$new(
    workers = 2,
    processes = 2,
    plan = future::sequential,
    timeout = 30
  )
  on.exit(x$shutdown())
  on.exit(processx::supervisor_kill(), add = TRUE)
  fun <- function(x) {
    Sys.sleep(0.1)
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
