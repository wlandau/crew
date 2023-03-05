crew_test("is_launcher()", {
  expect_true(is_launcher(crew_launcher_callr()))
  expect_false(is_launcher("x"))
})
