test_that("valid store class", {
  expect_silent(class_store$new()$validate())
})

test_that("invalid store class", {
  expect_silent(class_store$new(dir = NULL)$validate())
})

test_that("valid worker path", {
  x <- class_store$new()
  expect_equal(
    x$path_worker("abc"),
    file.path(x$dir, "worker", "abc")
  )
})

test_that("valid temp path", {
  x <- class_store$new()
  expect_equal(
    x$path_temp("abc"),
    file.path(x$dir, "temp", "abc")
  )
})
