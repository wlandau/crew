library(testthat)
library(crew)

temp_old <- list.files(tempdir())
test_check("crew")
if (!interactive() && !isTRUE(as.logical(Sys.getenv("NOT_CRAN", "false")))) {
  temp_new <- list.files(tempdir())
  unlink(setdiff(x = temp_new, y = temp_old), recursive = TRUE)
}
