if (FALSE) {
  eval(parse(text = "pkgload::load_all()"))
  root <- eval(parse(text = "rprojroot::find_package_root_file()"))
  testthat::test_dir(file.path(root, "tests", "local"))
}
