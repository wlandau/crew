test_that("empty crew", {
  expect_silent(class_crew$new()$validate())
})

test_that("nonempty crew", {
  worker <- class_worker$new()
  workers <- list(worker)
  names(workers) <- worker$name
  out <- class_crew$new(workers = workers)
  expect_silent(out$validate())
})

test_that("invalid crew", {
  workers <- list(a = 2)
  out <- class_crew$new(workers = workers)
  expect_error(out$validate(), class = "crew_error")
})
