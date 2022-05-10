crew_test("input", {
  x <- crew_store_local$new()
  worker <- "worker_x"
  expect_false(x$exists_input(worker = worker))
  expect_equal(x$list_input(), character(0))
  x$write_input(worker = worker, value = "value_x")
  expect_true(x$exists_input(worker))
  expect_equal(x$list_input(), worker)
  expect_equal(x$read_input(worker), "value_x")
  x$delete_input(worker = worker)
  expect_false(x$exists_input(worker = worker))
})

crew_test("output", {
  x <- crew_store_local$new()
  worker <- "worker_x"
  expect_false(x$exists_output(worker = worker))
  expect_equal(x$list_output(), character(0))
  x$write_output(worker = worker, value = "value_x")
  expect_true(x$exists_output(worker))
  expect_equal(x$list_output(), worker)
  expect_equal(x$read_output(worker), "value_x")
  x$delete_output(worker = worker)
  expect_false(x$exists_output(worker = worker))
})

crew_test("marshal", {
  x <- crew_store_local$new()
  out <- x$marshal()
  expect_true(is.character(out) && length(out) == 1L && nzchar(out))
  y <- eval(parse(text = out))
  expect_equal(class(x), class(y))
  expect_equal(x$get_root(), y$get_root())
})

crew_test("destroy", {
  x <- crew_store_local$new()
  worker <- "worker_x"
  task <- "task_x"
  expect_false(file.exists(x$get_root()))
  x$write_output(worker = worker, value = "x")
  expect_true(file.exists(x$get_root()))
  expect_true(x$exists_output(worker = worker))
  x$destroy()
  expect_false(file.exists(x$get_root()))
  expect_false(x$exists_output(worker = worker))
})
