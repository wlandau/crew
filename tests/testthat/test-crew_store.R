crew_test("get_root()", {
  tmp <- tempfile()
  x <- crew_store$new(root = tmp)
  expect_equal(x$get_root(), tmp)
})
