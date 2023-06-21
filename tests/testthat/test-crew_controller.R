crew_test("deprecate auto_scale", {
  skip_on_cran()
  expect_warning(
    crew_controller(
      client = crew_client(host = "127.0.0.1"),
      launcher = crew_launcher_local(),
      auto_scale = "demand"
    ),
    class = "crew_deprecate"
  )
})
