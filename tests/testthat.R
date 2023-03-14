library(testthat)
library(crew)

windows_ci <- isTRUE(as.logical(Sys.getenv("CI"))) &&
  identical(tolower(Sys.info()[["sysname"]]), "windows")

if (!windows_ci) {
  test_check("crew")
}
