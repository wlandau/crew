crew_test <- function(label, code) {
  expr <- substitute(
    testthat::test_that(label, code),
    env = list(label = label, code = substitute(code))
  )
  suppressMessages(eval(expr, envir = parent.frame()))
}

crew_test_sleep <- function() {
  Sys.sleep(2.25)
}

expect_crew_error <- function(object) {
  testthat::expect_error(object, class = "crew_error")
}
