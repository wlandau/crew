library(testthat)
library(crew)

if (packageVersion("nanonext") > "1.1.0") {
  test_check("crew")
}
