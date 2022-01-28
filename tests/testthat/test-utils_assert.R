test_that("crew_assert", {
  expect_silent(crew_assert(TRUE))
  expect_error(crew_assert(FALSE), class = "crew_error")
})
