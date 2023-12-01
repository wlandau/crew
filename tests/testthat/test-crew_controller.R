crew_test("deprecate auto_scale", {
  skip_on_cran()
  suppressWarnings(
    crew_controller(
      client = crew_client(host = "127.0.0.1"),
      launcher = crew_launcher_local(),
      auto_scale = "demand"
    )
  )
  expect_true(TRUE)
})

crew_test("crew_controller() deprecate collect()", {
  skip_on_cran()
  skip_on_os("windows")
  x <- crew_controller_local(seconds_idle = 360)
  suppressWarnings(x$collect())
  expect_true(TRUE)
})
