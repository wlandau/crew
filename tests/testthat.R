library(testthat)
library(crew)

test_check("crew")

# nolint start
# pkgload::load_all()
# for (dir in list.dirs("tests", recursive = FALSE)) {
#   message(dir)
#   testthat::test_dir(dir)
# }
# testthat::test_dir("tests/interactive")
# testthat::test_dir("tests/launchers")
# nolint end
