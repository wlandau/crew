test_that("valid worker", {
  crew <- class_crew$new()
  worker <- class_worker_callr$new(crew = crew)
  crew$workers[[worker$name]] <- worker
  expect_silent(worker$validate())
})

test_that("invalid worker", {
  expect_error(class_worker$new(name = NULL)$validate(), class = "crew_error")
})
