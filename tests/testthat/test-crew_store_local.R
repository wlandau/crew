test_that("worker input", {
  x <- crew_store_local$new()
  expect_false(x$exists_worker_input("worker"))
  x$write_worker_input("worker", value = "x")
  expect_true(x$exists_worker_input("worker"))
  expect_equal(x$read_worker_input("worker"), "x")
  x$delete_worker_input("worker")
  expect_false(x$exists_worker_input("worker"))
})

test_that("worker output", {
  x <- crew_store_local$new()
  expect_false(x$exists_worker_output("worker"))
  x$write_worker_output("worker", value = "x")
  expect_true(x$exists_worker_output("worker"))
  expect_equal(x$read_worker_output("worker"), "x")
  x$delete_worker_output("worker")
  expect_false(x$exists_worker_output("worker"))
})

test_that("upload input", {
  x <- crew_store_local$new()
  x$write_main_input("worker", value = "x")
  expect_true(x$exists_main_input("worker"))
  expect_false(x$exists_worker_input("worker"))
  x$upload_input("worker")
  expect_false(x$exists_main_input("worker"))
  expect_true(x$exists_worker_input("worker"))
})

test_that("download output", {
  x <- crew_store_local$new()
  x$write_worker_output("worker", value = "x")
  expect_true(x$exists_worker_output("worker"))
  expect_false(x$exists_main_output("worker"))
  x$download_output("worker")
  expect_false(x$exists_worker_output("worker"))
  expect_true(x$exists_main_output("worker"))
})

test_that("marshal", {
  x <- crew_store_local$new()
  out <- x$marshal()
  expect_true(is.character(out) && length(out) == 1L && nzchar(out))
  y <- eval(parse(text = out))
  expect_equal(class(x), class(y))
  expect_equal(x$get_root(), y$get_root())
})
