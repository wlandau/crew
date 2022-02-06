test_that("%||%", {
  expect_equal("a" %||% "b", "a")
  expect_equal(NULL %||% "b", "b")
})

test_that("if_any()", {
  expect_equal(if_any(TRUE, "a", "b"), "a")
  expect_equal(if_any(FALSE, "a", "b"), "b")
})

test_that("crew_wait with argless function", {
  expect_silent(crew_wait(fun = function() TRUE))
  expect_error(
    crew_wait(fun = function() FALSE, timeout = 0.05, wait = 0.01),
    class = "crew_timeout"
  )
})

test_that("crew_wait on a file", {
  tmp <- tempfile()
  fun <- function(x) file.exists(x)
  args <- list(x = tmp)
  expect_error(
    crew_wait(fun = fun, args = args, timeout = 0.05, wait = 0),
    class = "crew_timeout"
  )
  file.create(tmp)
  on.exit(unlink(tmp))
  expect_silent(crew_wait(fun = fun, args = args))
})

