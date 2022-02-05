test_that("dir_create()", {
  tmp <- file.path(tempfile(), "x", "y")
  expect_false(dir.exists(tmp))
  expect_false(file.exists(tmp))
  dir_create(tmp)
  expect_true(dir.exists(tmp))
  expect_true(file.exists(tmp))
  expect_silent(dir_create(tmp))
})
