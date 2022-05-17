crew_test("crew_catch_crash()", {
  expect_silent(crew_catch_crash("x"))
  expect_error(crew_catch_crash(stop()), class = "crew_error")
})

crew_test("crew_crash()", {
  expect_error(crew_crash("x"), class = "crew_error")
})

crew_test("crew_error()", {
  expect_error(crew_error("x"), class = "crew_error")
})

crew_test("crew_expire()", {
  expect_error(crew_expire("x"), class = "crew_expire")
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
