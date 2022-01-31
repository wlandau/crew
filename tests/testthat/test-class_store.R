test_that("valid store class", {
  expect_silent(class_store$new()$validate())
})

test_that("invalid store class", {
  expect_silent(class_store$new(root = NULL)$validate())
})

test_that("valid worker path", {
  x <- class_store$new()
  expect_equal(x$path("abc"), file.path(x$root, "worker", "abc"))
})

test_that("valid temp path", {
  x <- class_store$new()
  expect_equal(x$path_temp("abc"), file.path(x$root, "temp", "abc"))
})
