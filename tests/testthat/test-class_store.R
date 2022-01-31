test_that("valid store class", {
  expect_silent(class_store$new()$validate())
})

test_that("invalid store class", {
  expect_silent(class_store$new(dir_root = NULL)$validate())
})

test_that("input path", {
  x <- class_store$new()
  expect_equal(x$path_input("abc"), file.path(x$dir_root, "input", "abc"))
})

test_that("output path", {
  x <- class_store$new()
  expect_equal(x$path_output("abc"), file.path(x$dir_root, "output", "abc"))
})
