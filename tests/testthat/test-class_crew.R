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

test_that("crew recruit", {
  crew <- class_crew$new(
    store = crew::class_store_local$new(),
    worker_classes = list(
      crew::class_worker_callr,
      crew::class_worker_future
    )
  )
  crew$validate()
  crew$recruit(workers = 2)
  crew$validate()
  expect_equal(length(crew$workers), 2L)
  for (worker in crew$workers) {
    expect_true("process" %in% names(worker))
  }
  crew$recruit(workers = 3, class = "worker_future")
  crew$validate()
  expect_equal(length(crew$workers), 5L)
  for (index in seq(3, 5)) {
    expect_true("future" %in% names(crew$workers[[index]]))
  }
  walk(crew$workers, ~.x$validate())
})

test_that("crew launch", {
  crew <- class_crew$new()
  crew$recruit(2, timeout = Inf)
  crew$launch()
  walk(crew$workers, ~expect_true(.x$up()))
  walk(crew$workers, ~.x$shutdown())
})

test_that("crew sendable", {
  crew <- class_crew$new()
  expect_false(crew$sendable())
  crew$recruit(2, timeout = Inf)
  expect_true(crew$sendable())
  for (worker in crew$workers) {
    worker$assigned <- TRUE
  }
  expect_false(crew$sendable())
})

test_that("crew receivable", {
  crew <- class_crew$new()
  expect_false(crew$receivable())
  crew$recruit(2, timeout = Inf)
  expect_false(crew$receivable())
  crew$store$write_output(name = crew$workers[[2]]$name, data = "x")
  expect_true(crew$sendable())
})
  