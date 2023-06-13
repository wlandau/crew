crew_test("deprecate auto_scale", {
  skip_on_cran()
  expect_warning(
    crew_controller(
      client = crew_client(),
      launcher = crew_launcher_local(),
      auto_scale = "demand"
    ),
    class = "crew_deprecate"
  )
})
