crew_test("crew_true()", {
  expect_silent(crew_true(TRUE))
  expect_error(crew_true(FALSE), class = "crew_error")
  expect_silent(crew_true(c(2, 3), . > 1, . > 0))
  expect_error(crew_true(2, . < 1), class = "crew_error")
})
