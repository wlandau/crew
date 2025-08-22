crew_test("mirai_status()", {
  skip_on_cran()
  skip_on_os("windows")
  out <- mirai_status(
    profile = "none",
    seconds_interval = 1L,
    seconds_timeout = 15L
  )
  expect_true(is.list(out))
})

crew_test("mirai_status_error()", {
  skip_on_cran()
  expect_crew_error(mirai_status_error(status = 5L, profile = "default"))
})
