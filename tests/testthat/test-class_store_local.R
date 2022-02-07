crew_test("temp path", {
  skip_on_os("windows")
  x <- class_store_local$new()
  expect_equal(
    dirname(x$path_temp("abc")),
    dirname(file.path(x$dir_root, "temp", "abc"))
  )
})

crew_test("local store input", {
  x <- class_store_local$new()
  name <- "abc"
  data <- list(x = "x")
  expect_false(file.exists(x$path_input(name)))
  expect_false(x$exists_input(name))
  expect_error(x$read_input(name), class = "crew_error")
  x$write_input(name, data)
  expect_true(file.exists(x$path_input(name)))
  expect_true(x$exists_input(name))
  expect_equal(x$read_input(name), data)
  x$delete_input(name)
  expect_true(file.exists(x$dir_root))
  expect_false(file.exists(x$path_input(name)))
  expect_false(x$exists_input(name))
  x$destroy()
  expect_false(file.exists(x$dir_root))
})

crew_test("local store output", {
  x <- class_store_local$new()
  name <- "abc"
  data <- list(x = "x")
  expect_false(file.exists(x$path_output(name)))
  expect_false(x$exists_output(name))
  expect_error(x$read_output(name), class = "crew_error")
  x$write_output(name, data)
  expect_true(file.exists(x$path_output(name)))
  expect_true(x$exists_output(name))
  expect_equal(x$read_output(name), data)
  x$delete_output(name)
  expect_true(file.exists(x$dir_root))
  expect_false(file.exists(x$path_output(name)))
  expect_false(x$exists_output(name))
  x$destroy()
  expect_false(file.exists(x$dir_root))
})
