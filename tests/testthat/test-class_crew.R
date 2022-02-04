test_that("empty crew", {
  expect_silent(class_crew$new()$validate())
})

test_that("nonempty crew", {
  crew <- class_crew$new()
  worker <- class_worker_callr$new(crew = crew)
  crew$workers[[worker$name]] <- worker
  expect_silent(crew$validate())
})

test_that("invalid crew", {
  crew <- class_crew$new()
  worker <- class_worker_callr$new(crew = crew)
  crew$workers[["nope"]] <- worker
  expect_error(crew$validate(), class = "crew_error")
})
