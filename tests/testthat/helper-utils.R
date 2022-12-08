crew_test <- function(label, code) {
  expr <- substitute(
    testthat::test_that(label, code),
    env = list(label = label, code = substitute(code))
  )
  suppressMessages(eval(expr, envir = parent.frame()))
}
