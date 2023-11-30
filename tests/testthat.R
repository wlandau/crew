library(testthat)
library(crew)

test_check("crew")

# nolint start
# pkgload::load_all()
# dirs <- list.dirs("tests", recursive = FALSE)
# dirs <- dirs[!grepl("testthat", dirs)]
# for (dir in list.dirs("tests", recursive = FALSE)) {
#   message(dir)
#   testthat::test_dir(dir)
# }
# nolint end
