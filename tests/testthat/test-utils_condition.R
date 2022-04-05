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
