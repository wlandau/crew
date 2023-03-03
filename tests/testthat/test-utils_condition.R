crew_test("true()", {
  expect_silent(true(TRUE))
  expect_error(true(FALSE), class = "crew_error")
  expect_silent(true(c(2, 3), . > 1, . > 0))
  expect_error(true(2, . < 1), class = "crew_error")
})

crew_test("crew_error()", {
  expect_error(crew_error("x"), class = "crew_error")
})

crew_test("crew_expire()", {
  expect_error(crew_expire("x"), class = "crew_expire")
})

crew_test("crew_message()", {
  expect_message(crew_message("x"), class = "crew_message")
})

crew_test("crew_condition_false", {
  expect_false(
    tryCatch(
      stop("123"),
      error = crew_condition_false
    )
  )
})

crew_test("crew_condition_message", {
  expect_equal(
    tryCatch(
      stop("123"),
      error = crew_condition_message
    ),
    "123"
  )
})
