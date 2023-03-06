crew_test("abstract launcher class", {
  expect_silent(crew_class_launcher$new()$validate)
  expect_crew_error(crew_class_launcher$new(async_dial = -1)$validate())
})
