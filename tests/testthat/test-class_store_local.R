test_that("temp path", {
  x <- class_store_local$new()
  expect_equal(x$path_temp("abc"), file.path(x$dir_root, "temp", "abc"))
})

test_that("local store", {
  x <- class_store_local$new()
  name <- "abc"
  data <- list(x = "x")
  expect_false(file.exists(x$path(name)))
  expect_false(x$exists(name))
  expect_error(x$read(name), class = "crew_error")
  x$write(name, data)
  expect_true(file.exists(x$path(name)))
  expect_true(x$exists(name))
  expect_equal(x$read(name), data)
  x$delete(name)
  expect_true(file.exists(x$dir))
  expect_false(file.exists(x$path(name)))
  expect_false(x$exists(name))
  x$destroy()
  expect_false(file.exists(x$dir))
})
