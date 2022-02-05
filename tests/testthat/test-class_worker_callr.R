test_that("local callr worker works", {
  crew <- class_crew$new(worker_classes = list(default = class_worker_callr))
  x <- class_worker_callr$new(
    crew = crew,
    timeout = Inf,
    wait_input = 0.01
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

test_that("idempotent launch", {
  crew <- class_crew$new(worker_classes = list(default = class_worker_callr))
  x <- class_worker_callr$new(
    crew = crew,
    timeout = Inf,
    wait_input = 0.01
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

test_that("can send without explicit launch", {
  crew <- class_crew$new(worker_classes = list(default = class_worker_callr))
  x <- class_worker_callr$new(
    crew = crew,
    timeout = Inf,
    wait_input = 0.01
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

test_that("cannot send if worker is already assigned", {
  crew <- class_crew$new(worker_classes = list(default = class_worker_callr))
  x <- class_worker_callr$new(
    crew = crew,
    timeout = Inf,
    wait_input = 0.01
  )
  crew$workers[[x$name]] <- x
  x$assigned <- TRUE
  expect_error(
    x$send(fun = function(x) x + 1L, args = list(x = 1L)),
    class = "crew_error"
  )
})
