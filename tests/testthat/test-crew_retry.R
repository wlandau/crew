crew_test("crew_retry() with argless function", {
  expect_silent(crew_retry(fun = function() TRUE))
  expect_error(
    crew_retry(
      fun = function() FALSE,
      seconds_interval = 0.01,
      seconds_timeout = 0.05,
      error = TRUE
    ),
    class = "crew_expire"
  )
})

crew_test("crew_retry() with argless function and error = FALSE", {
  expect_silent(crew_retry(fun = function() TRUE))
  expect_silent(
    crew_retry(
      fun = function() FALSE,
      seconds_interval = 0.01,
      seconds_timeout = 0.05,
      error = FALSE
    )
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

crew_test("crew_retry() max_tries", {
  skip_on_cran()
  envir <- new.env(parent = emptyenv())
  envir$count <- 0L
  expect_error(
    crew_retry(
      fun = ~{
        envir$count <- envir$count + 1L
        FALSE
      },
      seconds_interval = 0.001,
      seconds_timeout = 60,
      max_tries = 5L
    ),
    class = "crew_expire"
  )
  expect_equal(envir$count, 5L)
})

crew_test("crew_retry() max_tries and error = FALSE", {
  skip_on_cran()
  envir <- new.env(parent = emptyenv())
  envir$count <- 0L
  expect_silent(
    crew_retry(
      fun = ~{
        envir$count <- envir$count + 1L
        FALSE
      },
      seconds_interval = 0.001,
      seconds_timeout = 60,
      max_tries = 5L,
      error = FALSE
    )
  )
  expect_equal(envir$count, 5L)
})
