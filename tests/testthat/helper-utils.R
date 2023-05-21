crew_test <- function(label, code) {
  expr <- substitute(
    testthat::test_that(label, code),
    env = list(label = label, code = substitute(code))
  )
  suppressMessages(eval(expr, envir = parent.frame()))
}

crew_test_sleep <- function() {
  on_windows <- identical(tolower(Sys.info()[["sysname"]]), "windows")
  on_cran <- !identical(Sys.getenv("NOT_CRAN"), "true")
  on_ci <- isTRUE(as.logical(Sys.getenv("CI")))
  if (on_windows || on_cran || on_ci) {
    Sys.sleep(2.25)
  } else {
    Sys.sleep(0.1)
  }
  invisible()
}

expect_crew_error <- function(object) {
  testthat::expect_error(object, class = "crew_error")
}

# TODO: always test and bump required versions when the next
# mirai/nanonext are on CRAN.
skip_if_low_dep_versions <- function() {
  sufficient_versions <- rlang::is_installed(
    pkg = c(
      "mirai (>= 0.8.7.9012)",
      "nanonext (>= 0.8.3.9007)"
    )
  )
  if (!sufficient_versions) {
    skip("version of mirai or nanonext is too low")
  }
}
