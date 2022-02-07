crew_test("valid store class", {
  expect_silent(class_store_local$new()$validate())
})

crew_test("invalid store class", {
  x <- class_store_local$new()
  x$dir_root <- NULL
  expect_error(x$validate(), class = "crew_error")
})

crew_test("input path", {
  x <- class_store$new()
  expect_equal(x$path_input("abc"), file.path(x$dir_root, "input", "abc"))
})

crew_test("output path", {
  x <- class_store$new()
  expect_equal(x$path_output("abc"), file.path(x$dir_root, "output", "abc"))
})
