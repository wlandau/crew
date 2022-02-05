test_that("local future worker works", {
  crew <- class_crew$new(worker_classes = list(class_worker_future))
  x <- class_worker_future$new(
    crew = crew,
    timeout = Inf,
    wait_input = 0.01
  )
  crew$workers[[x$name]] <- x
  x$validate()
  # no future yet
  expect_false(x$assigned)
  expect_false(x$up())
  expect_false(x$done())
  # launch future
  future::plan(future.callr::callr)
  on.exit(future::plan(future::sequential))
  x$launch()
  expect_true(x$up())
  expect_false(x$done())
  x$validate()
  # send first job
  expect_false(x$crew$store$exists_input(x$name))
  expect_false(x$crew$store$exists_output(x$name))
  x$send(fun = function(x) x + 1L, args = list(x = 1L))
  # Did the job run?
  tries <- 300
  while (tries > 0 && !x$done()) {
    Sys.sleep(0.1)
    tries <- tries - 1
  }
  expect_true(x$crew$store$exists_output(x$name))
  expect_true(x$done())
  expect_true(x$up())
  expect_true(x$assigned)
  expect_false(x$crew$store$exists_input(x$name))
  expect_true(x$crew$store$exists_output(x$name))
  # get results
  expect_equal(x$receive(), 2L)
  expect_false(x$assigned)
  # another job
  x$submit(fun = function(x) x + 1L, args = list(x = 2L))
  tries <- 300
  while (tries > 0 && !x$done()) {
    Sys.sleep(0.1)
    tries <- tries - 1
  }
  expect_equal(x$receive(), 3L)
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
  crew <- class_crew$new(worker_classes = list(class_worker_future))
  x <- class_worker_future$new(
    crew = crew,
    timeout = Inf,
    wait_input = 0.01
  )
  crew$workers[[x$name]] <- x
  future::plan(future.callr::callr)
  on.exit(future::plan(future::sequential))
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
