library(testthat)
library(crew)

test_check("crew")
if (identical(tolower(Sys.info()[["sysname"]]), "windows")) {
  Sys.sleep(5)
}
