if (FALSE) {
  eval(parse(text = "pkgload::load_all()"))
  root <- eval(parse(text = "rprojroot::find_package_root_file()"))
  dirs <- list.dirs(file.path(root, "tests"), recursive = FALSE)
  dirs <- dirs[!grepl("testthat", dirs)]
  for (dir in dirs) {
    message(dir)
    testthat::test_dir(dir)
  }
}
