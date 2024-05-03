# {later} async has a hard time inside functions that do not relinquish
# the main event loop, so these tests need to run interactively line-by-line.
crew_test("interactive: promise(mode = \"one\") on controller groups", {
  a <- crew_controller_local(
    name = "a",
    workers = 1L,
    seconds_idle = 360
  )
  b <- crew_controller_local(
    name = "b",
    workers = 1L,
    seconds_idle = 360
  )
  x <- crew_controller_group(a, b)
  x$start()
  expect_null(a$autoscaling)
  expect_null(b$autoscaling)
  envir <- new.env(parent = emptyenv())
  # Test a good task.
  x$push("done", controller = "b")
  x$wait(mode = "one")
  promise <- promises::then(
    suppressWarnings(x$promise(mode = "one")),
    onFulfilled = function(value) {
      envir$value <- value
    },
    onRejected = function(error) {
      envir$error <- conditionMessage(error)
    }
  )
  expect_true(a$autoscaling)
  expect_true(b$autoscaling)
  expect_true(tibble::is_tibble(envir$value))
  expect_equal(nrow(envir$value), 1L)
  expect_equal(envir$value$result[[1L]], "done")
  expect_null(envir$error)
  envir <- new.env(parent = emptyenv())
  # Test an errored task.
  x$push(stop("error message"))
  promise <- promises::then(
    suppressWarnings(x$promise(mode = "one")),
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
  expect_false(a$autoscaling)
  expect_false(b$autoscaling)
})

crew_test("interactive: promise(mode = \"all\") on controller groups", {
  a <- crew_controller_local(
    name = "a",
    workers = 1L,
    seconds_idle = 360
  )
  b <- crew_controller_local(
    name = "b",
    workers = 1L,
    seconds_idle = 360
  )
  x <- crew_controller_group(a, b)
  x$start()
  x$push("done1", controller = "a")
  x$push("done2", controller = "a")
  envir <- new.env(parent = emptyenv())
  # Test on good tasks.
  promise <- promises::then(
    suppressWarnings(x$promise(mode = "all")),
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
  x$push("good", controller = "a")
  x$push(stop("error message"), controller = "b")
  promise <- promises::then(
    suppressWarnings(x$promise(mode = "all")),
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
  expect_true(a$autoscaling)
  x$descale()
  expect_false(a$autoscaling)
  x$terminate()
})
