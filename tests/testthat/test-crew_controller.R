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

crew_test("controller_map_message_progress()", {
  expect_silent(controller_map_message_progress(1, 2, FALSE))
  expect_message(controller_map_message_progress(1, 2, TRUE))
})

crew_test("controller_map_message_complete()", {
  expect_silent(controller_map_message_complete(100, 10, FALSE))
  expect_message(controller_map_message_complete(100, 10, TRUE))
})
