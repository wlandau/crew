crew_test("%||%", {
  expect_equal("a" %||% "b", "a")
  expect_equal(list() %||% "b", "b")
  expect_equal(NULL %||% "b", "b")
})

crew_test("%|||%", {
  expect_equal("a" %|||% "b", "a")
  expect_equal(list() %|||% "b", list())
  expect_equal(NULL %|||% "b", "b")
})

crew_test("if_any()", {
  expect_equal(if_any(TRUE, "a", "b"), "a")
  expect_equal(if_any(FALSE, "a", "b"), "b")
})
