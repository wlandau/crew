test_that("local future worker works", {
  crew <- class_crew$new(worker_definition = class_worker_future_local)
  x <- class_worker_future_local$new(
    crew = crew,
    timeout = Inf,
    wait_input = 0.01
  )
  crew$worker_list[[x$name]] <- x
  x$validate()
  # no future yet
  expect_false(x$alive())
  expect_false(x$receivable())
  # launch future
  future::plan(future.callr::callr)
  on.exit(future::plan(future::sequential))
  x$launch()
  expect_true(x$alive())
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
  expect_true(x$alive())
  expect_false(x$crew$store$exists_input(x$name))
  expect_true(x$crew$store$exists_output(x$name))
  # get results
  expect_equal(x$receive(), 2L)
  # another job
  x$send(fun = function(x) x + 1L, args = list(x = 2L))
  tries <- 300
  while (tries > 0 && !x$receivable()) {
    Sys.sleep(0.1)
    tries <- tries - 1
  }
  expect_equal(x$receive(), 3L)
  # termination
  x$shutdown()
  tries <- 300
  while (tries > 0 && x$alive()) {
    Sys.sleep(0.1)
    tries <- tries - 1
  }
  expect_false(x$alive())
})

test_that("cover idempotent launch", {
  crew <- class_crew$new(worker_definition = class_worker_future_local)
  x <- class_worker_future_local$new(
    crew = crew,
    timeout = Inf,
    wait_input = 0.01
  )
  crew$worker_list[[x$name]] <- x
  future::plan(future.callr::callr)
  on.exit(future::plan(future::sequential))
  x$launch()
  x$launch()
  x$shutdown()
  tries <- 300
  while (tries > 0 && x$alive()) {
    Sys.sleep(0.1)
    tries <- tries - 1
  }
  expect_false(x$alive())
})
