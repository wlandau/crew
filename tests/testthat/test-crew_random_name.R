crew_test("crew_random_name()", {
  skip_if_low_dep_versions()
  out <- crew_random_name()
  expect_true(is.character(out))
  expect_false(anyNA(out))
  expect_equal(length(out), 1L)
  expect_gt(nzchar(out), 0L)
})
