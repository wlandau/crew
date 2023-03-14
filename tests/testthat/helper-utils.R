crew_test <- function(label, code) {
  expr <- substitute(
    testthat::test_that(label, code),
    env = list(label = label, code = substitute(code))
  )
  suppressMessages(eval(expr, envir = parent.frame()))
  if (identical(tolower(Sys.info()[["sysname"]]), "windows")) {
    Sys.sleep(3L)
  }
}

expect_crew_error <- function(object) {
  testthat::expect_error(object, class = "crew_error")
}

skip_redis <- function() {
  skip_on_cran()
  if (!file.exists(redis_server_default_binary())) {
    skip("Redis not installed.")
  }
}
