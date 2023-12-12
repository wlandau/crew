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

crew_test("%||NA%", {
  expect_equal("a" %||NA% "b", "a")
  expect_equal(list() %||NA% "b", list())
  expect_null(NULL %||NA% "b")
  expect_equal(NA %||NA% "b", "b")
})

crew_test("if_any()", {
  expect_equal(if_any(TRUE, "a", "b"), "a")
  expect_equal(if_any(FALSE, "a", "b"), "b")
})

crew_test("crew_null", {
  expect_equal(length(crew_null), 1L)
  expect_true(is_crew_null(crew_null))
  expect_false(is_crew_null(NULL))
  expect_false(is_crew_null(NA))
  expect_false(is_crew_null(NA_character_))
  expect_false(is_crew_null(NA_real_))
  expect_false(is_crew_null(NA_integer_))
  expect_false(is_crew_null(TRUE))
  expect_false(is_crew_null(FALSE))
  expect_false(is_crew_null(list()))
  expect_false(is_crew_null(0L))
  expect_false(is_crew_null(1L))
  expect_false(is_crew_null("null"))
  expect_false(is_crew_null("NULL"))
})
