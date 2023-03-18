crew_test("crew_wait() with argless function", {
  expect_silent(crew_wait(fun = function() TRUE))
  expect_error(
    crew_wait(
      fun = function() FALSE,
      seconds_interval = 0.01,
      seconds_timeout = 0.05
    ),
    class = "crew_expire"
  )
})

crew_test("crew_wait() on a file", {
  skip_on_cran()
  tmp <- tempfile()
  fun <- function(x) file.exists(x)
  args <- list(x = tmp)
  expect_error(
    crew_wait(
      fun = ~FALSE,
      args = args,
      seconds_interval = 0.01,
      seconds_timeout = 0.05
    ),
    class = "crew_expire"
  )
  file.create(tmp)
  on.exit(unlink(tmp))
  expect_silent(crew_wait(fun = fun, args = args))
})
