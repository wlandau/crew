crew_test("random_name()", {
  out <- random_name()
  expect_true(is.character(out))
  expect_false(anyNA(out))
  expect_equal(length(out), 1L)
  expect_gt(nzchar(out), 0L)
})

crew_test("is_named", {
  expect_true(is_named(NULL))
  expect_true(is_named(list()))
  expect_true(is_named(list(a = 1)))
  expect_true(is_named(list(a = 1, b = 1)))
  expect_false(is_named(list(a = 1, a = 1)))
  expect_false(is_named(list(a = 1, 2)))
})
