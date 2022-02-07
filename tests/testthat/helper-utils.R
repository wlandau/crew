crew_test <- function(label, code) {
  expr <- substitute(
    testthat::test_that(label, code),
    env = list(label = label, code = substitute(code))
  )
  eval(expr, envir = parent.frame())
  processx::supervisor_kill()
}
