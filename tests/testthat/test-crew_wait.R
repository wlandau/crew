crew_test("crew_retry() with argless function", {
  expect_silent(crew_retry(fun = function() TRUE))
  expect_error(
    crew_retry(
      fun = function() FALSE,
      seconds_interval = 0.01,
      seconds_timeout = 0.05
    ),
    class = "crew_expire"
  )
})

crew_test("crew_retry() on a file", {
  skip_on_cran()
  tmp <- tempfile()
  fun <- function(x) file.exists(x)
  args <- list(x = tmp)
  expect_error(
    crew_retry(
      fun = ~FALSE,
      args = args,
      seconds_interval = 0.01,
      seconds_timeout = 0.05
    ),
    class = "crew_expire"
  )
  file.create(tmp)
  on.exit(unlink(tmp))
  expect_silent(crew_retry(fun = fun, args = args))
})
