crew_test("%||%", {
  expect_equal("a" %||% "b", "a")
  expect_equal(list() %||% "b", "b")
  expect_equal(NULL %||% "b", "b")
})

crew_test("%|||%", {
  expect_equal("a" %|||% "b", "a")
  expect_equal(list() %|||% "b", list())
  expect_equal(NULL %|||% "b", "b")
})

crew_test("if_any()", {
  expect_equal(if_any(TRUE, "a", "b"), "a")
  expect_equal(if_any(FALSE, "a", "b"), "b")
})

crew_test("crew_wait with argless function", {
  expect_silent(crew_wait(fun = function() TRUE))
  expect_error(
    crew_wait(fun = function() FALSE, timeout = 0.05, wait = 0.01),
    class = "crew_expire"
  )
})

crew_test("crew_wait on a file", {
  tmp <- tempfile()
  fun <- function(x) file.exists(x)
  args <- list(x = tmp)
  expect_error(
    crew_wait(fun = fun, args = args, timeout = 0.05, wait = 0),
    class = "crew_expire"
  )
  file.create(tmp)
  on.exit(unlink(tmp))
  expect_silent(crew_wait(fun = fun, args = args))
})
