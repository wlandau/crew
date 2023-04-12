library(testthat)
library(crew)

test_check("crew", reporter = ProgressReporter$new())
