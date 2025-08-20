crew_test("crew_controller_sequential() method and signature compatibility", {
  x <- crew_controller_local()
  y <- crew_controller_sequential()
  expect_equal(
    setdiff(sort(names(x)), "super"),
    setdiff(sort(names(y)), "super")
  )
  for (field in setdiff(names(x), "super")) {
    function_x <- is.function(x[[field]])
    function_y <- is.function(y[[field]])
    expect_equal(function_x, function_y)
    if (function_x) {
      expect_equal(names(formals(x[[field]])), names(formals(y[[field]])))
    }
  }
})

test_that("basic methods", {
  x <- crew_controller_sequential()
  expect_null(x$start())
  expect_equal(x$resolved(), 0L)
  expect_equal(x$unresolved(), 0L)
  expect_false(x$saturated())
  expect_null(x$launch())
  expect_false(x$scale())
  expect_null(x$autoscale())
  expect_null(x$descale())
  expect_true(x$wait())
  expect_null(x$push_backlog())
  expect_equal(x$pop_backlog(), character(0L))
  expect_null(x$cancel())
  expect_null(x$terminate())
})

test_that("basic task", {
  x <- crew_controller_sequential()
  x$start()
  x$push("task1")
  x$push("task2")
  expect_equal(x$resolved(), 2L)
  expect_equal(x$unresolved(), 0L)
  task1 <- x$pop()
  task2 <- x$pop()
  expect_null(x$pop())
  expect_equal(
    sort(as.character(c(task1$result, task2$result))),
    sort(c("task1", "task2"))
  )
  x$terminate()
  expect_equal(x$resolved(), 0L)
})

test_that("walk() and collect()", {
  x <- crew_controller_sequential()
  x$walk(x + 1L, iterate = list(x = c(1L, 2L)))
  expect_equal(
    sort(as.integer(x$collect()$result)),
    c(2L, 3L)
  )
})

test_that("map()", {
  x <- crew_controller_sequential()
  out <- x$map(x + 1L, iterate = list(x = c(1L, 2L)))
  expect_equal(
    sort(as.integer(out$result)),
    c(2L, 3L)
  )
})
