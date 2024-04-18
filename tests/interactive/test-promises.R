# {later} async has a hard time inside functions that do not relinquish
# the main event loop, so these tests need to run interactively line-by-line.
crew_test("interactive: promise(mode = \"one\") on basic controllers", {
  x <- crew_controller_local(
    workers = 1L,
    seconds_idle = 360
  )
  x$start()
  expect_null(x$autoscaling)
  envir <- new.env(parent = emptyenv())
  # Test a good task.
  x$push("done")
  promise <- promises::then(
    x$promise(mode = "one"),
    onFulfilled = function(value) {
      envir$value <- value
    },
    onRejected = function(error) {
      envir$error <- conditionMessage(error)
    }
  )
  expect_true(x$autoscaling)
  x$wait(mode = "one")
  expect_true(tibble::is_tibble(envir$value))
  expect_equal(nrow(envir$value), 1L)
  expect_equal(envir$value$result[[1L]], "done")
  expect_null(envir$error)
  envir <- new.env(parent = emptyenv())
  # Test an errored task.
  x$push(stop("error message"))
  promise <- promises::then(
    x$promise(mode = "one"),
    onFulfilled = function(value) {
      envir$value <- value
    },
    onRejected = function(error) {
      envir$error <- conditionMessage(error)
    }
  )
  x$wait(mode = "one")
  expect_equal(envir$error, "error message")
  expect_null(envir$value)
  x$terminate()
  expect_false(x$autoscaling)
})

crew_test("interactive: promise(mode = \"all\") on basic controllers", {
  x <- crew_controller_local(
    workers = 1L,
    seconds_idle = 360
  )
  x$start()
  x$push("done1")
  Sys.sleep(0.1)
  x$push("done2")
  envir <- new.env(parent = emptyenv())
  # Test on good tasks.
  promise <- promises::then(
    x$promise(mode = "all"),
    onFulfilled = function(value) {
      envir$value <- value
    },
    onRejected = function(error) {
      envir$error <- conditionMessage(error)
    }
  )
  x$wait(mode = "all")
  expect_true(tibble::is_tibble(envir$value))
  expect_equal(nrow(envir$value), 2L)
  expect_equal(
    sort(as.character(envir$value$result)),
    sort(c("done1", "done2"))
  )
  expect_null(envir$error)
  envir <- new.env(parent = emptyenv())
  # Test with an errored task.
  x$push("good")
  x$push(stop("error message"))
  promise <- promises::then(
    x$promise(mode = "all"),
    onFulfilled = function(value) {
      envir$value <- value
    },
    onRejected = function(error) {
      envir$error <- conditionMessage(error)
    }
  )
  x$wait(mode = "all")
  expect_equal(envir$error, "error message")
  expect_null(envir$value)
  expect_true(x$autoscaling)
  x$descale()
  expect_false(x$autoscaling)
  x$terminate()
})
