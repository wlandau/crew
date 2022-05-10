crew_test("crew_queue_future_worker_start()", {
  fun <- function(x) x
  args <- list(x = "x")
  value <- list(fun = deparse(fun), args = args)
  store <- crew_store_local$new()
  store$write_input(worker = "worker", value = value)
  out <- crew_queue_future_worker_start(
    worker = "worker",
    store = store$marshal(),
    timeout = 30,
    wait = 0.01,
    plan = future::sequential,
    task = "task"
  )
  expect_null(out$resolved)
  future <- out$future
  expect_null(future::value(future))
  expect_equal(store$read_output("worker")$result, "x")
})

crew_test("crew_queue_future_worker_resolve() detects crashes", {
  future::plan(future::sequential)
  handle <- list(
    future = future::future("x"),
    task = "x"
  )
  expect_error(
    crew_queue_future_worker_resolve(
      handle = handle,
      worker = "x_worker",
      store = crew_store_local$new()$marshal(),
      timeout = -1
    )
  )
})

crew_test("crew_queue_future_worker_resolve() gives time to time out", {
  future::plan(future::sequential)
  handle <- list(
    future = future::future("x_value"),
    task = "x_task"
  )
  out <- crew_queue_future_worker_resolve(
    handle = handle,
    worker = "x_worker",
    store = crew_store_local$new()$marshal(),
    timeout = Inf
  )
  expect_s3_class(out$future, "Future")
  expect_equal(out$task, "x_task")
  expect_s3_class(out$time_resolved, "POSIXct")
  expect_equal(out$value, "x_value")
  expect_true(out$checked_value)
  expect_false(out$resolved)
})

crew_test("crew_queue_future_worker_resolve() needs output to resolve", {
  future::plan(future::sequential)
  handle <- list(
    future = future::future("x_value"),
    task = "x_task"
  )
  store <- crew_store_local$new()
  worker <- "x_worker"
  store$write_output(worker = worker, value = "x_output")
  out <- crew_queue_future_worker_resolve(
    handle = handle,
    worker = worker,
    store = store$marshal(),
    timeout = Inf
  )
  expect_s3_class(out$future, "Future")
  expect_equal(out$task, "x_task")
  expect_s3_class(out$time_resolved, "POSIXct")
  expect_null(out$value)
  expect_null(out$checked_value)
  expect_true(out$resolved)
})

crew_test("get plan", {
  x <- crew_queue_future$new(workers = 2, plan = future::sequential)
  on.exit(x$shutdown())
  on.exit(processx::supervisor_kill(), add = TRUE)
  expect_s3_class(x$get_plan(), "sequential")
})

crew_test("detect crash", {
  future::plan(future::sequential)
  x <- crew_queue_future$new(
    workers = 1,
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

crew_test("custom plan", {
  future::plan(future::sequential)
  x <- crew_queue_future$new(
    workers = 1,
    plan = future::sequential
  )
  on.exit(x$shutdown())
  on.exit(processx::supervisor_kill(), add = TRUE)
  expect_false(file.exists("nope"))
  plan <- future::tweak(future.batchtools::batchtools_slurm, template = "nope")
  x$push(fun = function() "x", plan = plan)
  crew_wait(
    ~tryCatch({
        x$pop()
        FALSE
      },
      crew_error = function(condition) {
        TRUE
      }
    ),
    timeout = 30,
    wait = 0.1
  )
})

crew_test("worker up", {
  future::plan(future::sequential)
  x <- crew_queue_future$new(
    workers = 2,
    plan = future::sequential
  )
  on.exit(x$shutdown())
  on.exit(processx::supervisor_kill(), add = TRUE)
  worker <- x$get_workers()$worker[1]
  x$private$workers$done <- TRUE
  expect_true(x$private$worker_up(list(), worker))
  x$private$workers$done <- FALSE
  expect_false(x$private$worker_up(list(), worker))
  expect_false(
    x$private$worker_up(list(future = "x", resolved = TRUE), worker)
  )
  handle <- list(future = future::future("x"), resolved = FALSE)
  expect_true(x$private$worker_up(handle, worker), worker)
})

crew_test("private methods to submit and update_results work", {
  future::plan(future::sequential)
  x <- crew_queue_future$new(
    workers = 2,
    timeout = 30,
    plan = future::sequential,
    subqueue = crew_queue_session$new(workers = 2)
  )
  on.exit(x$shutdown())
  on.exit(processx::supervisor_kill(), add = TRUE)
  for (index in seq_len(2)) {
    input <- list(fun = function(x) x, args = list(x = index))
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
  x <- crew_queue_future$new(
    workers = 2,
    subqueue = crew_queue_session$new(workers = 2),
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

crew_test("push and pop", {
  future::plan(future::sequential)
  x <- crew_queue_future$new(
    workers = 2,
    subqueue = crew_queue_session$new(workers = 2),
    plan = future::sequential,
    timeout = 30
  )
  on.exit(x$shutdown())
  on.exit(processx::supervisor_kill(), add = TRUE)
  fun <- function(x) {
    Sys.sleep(0.1)
    x
  }
  done <- rep(FALSE, 10)
  for (index in seq_len(10)) {
    x$push(fun = fun, args = list(x = index))
    out <- x$pop()
    if (!is.null(out)) {
      done[out$result$result] <- TRUE
    }
  }
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
