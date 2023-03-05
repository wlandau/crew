crew_test("is_controller()", {
  expect_true(is_controller(crew_controller_callr()))
  expect_false(is_controller("x"))
})

crew_test("is_router()", {
  expect_true(is_router(crew_router()))
  expect_false(is_router("x"))
})

crew_test("is_launcher()", {
  expect_true(is_launcher(crew_launcher_callr()))
  expect_false(is_launcher("x"))
})
