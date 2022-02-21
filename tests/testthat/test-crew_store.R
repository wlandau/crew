test_that("get_root()", {
  tmp <- tempfile()
  x <- crew_store$new(dir_root = tmp)
  expect_equal(x$get_root(), tmp)
})

test_that("main input", {
  x <- crew_store$new()
  expect_false(x$exists_main_input("worker"))
  x$write_main_input("worker", value = "x")
  expect_true(x$exists_main_input("worker"))
  expect_equal(x$read_main_input("worker"), "x")
  x$delete_main_input("worker")
  expect_false(x$exists_main_input("worker"))
})

test_that("main output", {
  x <- crew_store$new()
  expect_false(x$exists_main_output("worker"))
  x$write_main_output("worker", value = "x")
  expect_true(x$exists_main_output("worker"))
  expect_equal(x$read_main_output("worker"), "x")
  x$delete_main_output("worker")
  expect_false(x$exists_main_output("worker"))
})

test_that("destroy", {
  x <- crew_store$new()
  expect_false(file.exists(x$get_root()))
  x$write_main_output("worker", value = "x")
  expect_true(file.exists(x$get_root()))
  expect_true(x$exists_main_output("worker"))
  x$destroy()
  expect_false(file.exists(x$get_root()))
  expect_false(x$exists_main_output("worker"))
})
