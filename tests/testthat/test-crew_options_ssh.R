test_that("crew_options_ssh()", {
  expect_equal(
    crew_options_ssh(host = "10.0.0.4"),
    structure(
      list(host = "10.0.0.4", port = 22L),
      class = c("crew_options_ssh", "crew_options")
    )
  )
})
