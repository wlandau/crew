crew_test("crew_user()", {
  skip_on_cran()
  user <- crew_user()
  expect_true(is.null(user) || is.character(user))
})
