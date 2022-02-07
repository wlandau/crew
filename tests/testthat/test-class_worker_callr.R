crew_test("local callr worker works", {
  crew <- class_crew$new(worker_classes = list(default = class_worker_callr))
  x <- class_worker_callr$new(
    crew = crew,
    timeout = Inf,
    wait = 0.01
  )
  crew$workers[[x$name]] <- x
  x$validate()
  # no callr yet
  expect_false(x$up())
  expect_false(x$receivable())
  # launch callr
  x$launch()
  expect_true(x$up())
  expect_false(x$receivable())
  x$validate()
  # send first job
  expect_false(x$crew$store$exists_input(x$name))
  expect_false(x$crew$store$exists_output(x$name))
  x$send(fun = function(x) x + 1L, args = list(x = 1L))
  # Did the job run?
  tries <- 300
  while (tries > 0 && !x$receivable()) {
    Sys.sleep(0.1)
    tries <- tries - 1
  }
  expect_true(x$crew$store$exists_output(x$name))
  expect_true(x$receivable())
  expect_true(x$up())
  expect_false(x$crew$store$exists_input(x$name))
  expect_true(x$crew$store$exists_output(x$name))
  # get results
  expect_equal(x$receive()$value, 2L)
  expect_false(x$crew$store$exists_input(x$name))
  expect_false(x$crew$store$exists_output(x$name))
  # another job
  x$send(fun = function(x) x + 1L, args = list(x = 2L))
  tries <- 300
  while (tries > 0 && !x$receivable()) {
    Sys.sleep(0.1)
    tries <- tries - 1
  }
  expect_equal(x$receive()$value, 3L)
  # termination
  x$shutdown()
  tries <- 300
  while (tries > 0 && x$up()) {
    Sys.sleep(0.1)
    tries <- tries - 1
  }
  expect_false(x$up())
})

crew_test("idempotent launch", {
  crew <- class_crew$new(worker_classes = list(default = class_worker_callr))
  x <- class_worker_callr$new(
    crew = crew,
    timeout = Inf,
    wait = 0.01
  )
  crew$workers[[x$name]] <- x
  x$launch()
  x$launch()
  x$shutdown()
  tries <- 300
  while (tries > 0 && x$up()) {
    Sys.sleep(0.1)
    tries <- tries - 1
  }
  expect_false(x$up())
})

crew_test("can send without explicit launch", {
  crew <- class_crew$new(worker_classes = list(default = class_worker_callr))
  x <- class_worker_callr$new(
    crew = crew,
    timeout = Inf,
    wait = 0.01
  )
  crew$workers[[x$name]] <- x
  expect_false(x$crew$store$exists_input(x$name))
  expect_false(x$crew$store$exists_output(x$name))
  x$send(fun = function(x) x + 1L, args = list(x = 1L))
  tries <- 300
  while (tries > 0 && !x$receivable()) {
    Sys.sleep(0.1)
    tries <- tries - 1
  }
  expect_true(x$crew$store$exists_output(x$name))
  expect_true(x$receivable())
  expect_true(x$up())
  expect_false(x$crew$store$exists_input(x$name))
  expect_true(x$crew$store$exists_output(x$name))
})

crew_test("cannot send if worker is already assigned", {
  crew <- class_crew$new(worker_classes = list(default = class_worker_callr))
  x <- class_worker_callr$new(
    crew = crew,
    timeout = Inf,
    wait = 0.01
  )
  crew$workers[[x$name]] <- x
  x$assigned <- TRUE
  expect_error(
    x$send(fun = function(x) x + 1L, args = list(x = 1L)),
    class = "crew_error"
  )
})

crew_test("value received", {
  crew <- class_crew$new()
  on.exit(crew$shutdown())
  crew$recruit(workers = 1, timeout = Inf)
  crew$send(fun = function(x) x, args = list(x = "y"))
  while (!crew$receivable()) {
    Sys.sleep(0.1)
  }
  out <- crew$receive()
  expect_equal(out$value, "y")
  expect_true(is.numeric(out$seconds))
  expect_equal(length(out$seconds), 1)
  expect_null(out$error)
  expect_null(out$traceback)
  expect_null(out$warnings)
})

crew_test("value with warnings and errors", {
  crew <- class_crew$new()
  on.exit(crew$shutdown())
  crew$recruit(workers = 1, timeout = Inf)
  crew$send(
    fun = function() {
      warning("warning1")
      warning("warning2")
      stop("error1")
    }
  )
  while (!crew$receivable()) {
    Sys.sleep(0.1)
  }
  out <- crew$receive()
  expect_null(out$value)
  expect_true(is.numeric(out$seconds))
  expect_equal(length(out$seconds), 1)
  expect_equal(out$error, "error1")
  expect_true(is.character(out$traceback) && length(out$traceback) > 0)
  expect_equal(out$warnings, c("warning1", "warning2"))
  expect_true(crew$workers[[1]]$up())
})

crew_test("restart one stuck workers", {
  crew <- class_crew$new()
  on.exit(crew$shutdown())
  crew$recruit(workers = 2, timeout = Inf)
  for (worker in crew$workers) {
    expect_false(worker$stuck())
  }
  for (index in seq_len(2)) {
    crew$send(fun = function() Sys.sleep(Inf))
  }
  for (worker in crew$workers) {
    expect_true(worker$up())
    expect_false(worker$stuck())
    worker$process$kill()
    while (worker$up()) Sys.sleep(0.1)
    expect_false(worker$up())
    expect_true(worker$stuck())
  }
  crew$restart(workers = 1)
  expect_true(crew$workers[[1]]$up())
  expect_false(crew$workers[[1]]$stuck())
  expect_false(crew$workers[[2]]$up())
  expect_true(crew$workers[[2]]$stuck())
})

crew_test("restart the tagged stuck worker", {
  crew <- class_crew$new()
  on.exit(crew$shutdown())
  crew$recruit(workers = 1, timeout = Inf, tags = "a")
  crew$recruit(workers = 1, timeout = Inf, tags = "b")
  for (worker in crew$workers) {
    expect_false(worker$stuck())
  }
  for (index in seq_len(2)) {
    crew$send(fun = function() Sys.sleep(Inf))
  }
  for (worker in crew$workers) {
    expect_true(worker$up())
    expect_false(worker$stuck())
    worker$process$kill()
    while (worker$up()) Sys.sleep(0.1)
    expect_false(worker$up())
    expect_true(worker$stuck())
  }
  crew$restart(tags = "b")
  expect_false(crew$workers[[1]]$up())
  expect_true(crew$workers[[1]]$stuck())
  expect_true(crew$workers[[2]]$up())
  expect_false(crew$workers[[2]]$stuck())
})

crew_test("restart two stuck workers", {
  crew <- class_crew$new()
  on.exit(crew$shutdown())
  crew$recruit(workers = 2, timeout = Inf)
  for (worker in crew$workers) {
    expect_false(worker$stuck())
  }
  for (index in seq_len(2)) {
    crew$send(fun = function() Sys.sleep(Inf))
  }
  for (worker in crew$workers) {
    expect_true(worker$up())
    expect_false(worker$stuck())
    worker$process$kill()
    while (worker$up()) Sys.sleep(0.1)
    expect_false(worker$up())
    expect_true(worker$stuck())
  }
  crew$restart(workers = 2)
  for (worker in crew$workers) {
    expect_true(worker$up())
    expect_false(worker$stuck())
  }
})

crew_test("restart the second worker", {
  crew <- class_crew$new()
  on.exit(crew$shutdown())
  crew$recruit(workers = 2, timeout = Inf)
  for (worker in crew$workers) {
    expect_false(worker$stuck())
  }
  for (index in seq_len(2)) {
    crew$send(fun = function() Sys.sleep(Inf))
  }
  worker <- crew$workers[[2]]
  expect_true(worker$up())
  expect_false(worker$stuck())
  worker$process$kill()
  while (worker$up()) Sys.sleep(0.1)
  expect_false(worker$up())
  expect_true(worker$stuck())
  crew$restart(workers = 2)
  for (worker in crew$workers) {
    expect_true(worker$up())
    expect_false(worker$stuck())
  }
})

crew_test("restart the second worker", {
  crew <- class_crew$new()
  on.exit(crew$shutdown())
  crew$recruit(workers = 2, timeout = Inf)
  for (worker in crew$workers) {
    expect_false(worker$stuck())
  }
  for (index in seq_len(2)) {
    crew$send(fun = function() Sys.sleep(Inf))
  }
  worker <- crew$workers[[2]]
  expect_true(worker$up())
  expect_false(worker$stuck())
  worker$process$kill()
  while (worker$up()) Sys.sleep(0.1)
  expect_false(worker$up())
  expect_true(worker$stuck())
  crew$restart(workers = 2)
  for (worker in crew$workers) {
    expect_true(worker$up())
    expect_false(worker$stuck())
  }
})
