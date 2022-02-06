test_that("valid worker", {
  crew <- class_crew$new()
  worker <- class_worker_callr$new(crew = crew)
  crew$workers[[worker$name]] <- worker
  expect_silent(worker$validate())
})

test_that("invalid worker", {
  expect_error(class_worker$new(name = NULL)$validate(), class = "crew_error")
})

test_that("worker tagged", {
  crew <- class_crew$new()
  worker <- class_worker_callr$new(crew = crew, tags = c("t1", "t2", "t3"))
  crew$workers[[worker$name]] <- worker
  expect_silent(worker$validate())
  expect_true(worker$tagged("t1"))
  expect_true(worker$tagged(c("t1", "t3", "t4")))
  expect_false(worker$tagged("nope"))
})

test_that("worker clear", {
  crew <- class_crew$new()
  worker <- class_worker_callr$new(crew = crew, tags = c("t1", "t2", "t3"))
  crew$workers[[worker$name]] <- worker
  expect_silent(worker$validate())
  crew$store$write_input(worker$name, "x")
  crew$store$write_output(worker$name, "x")
  expect_true(crew$store$exists_input(worker$name))
  expect_true(crew$store$exists_output(worker$name))
  worker$clear()
  expect_false(crew$store$exists_input(worker$name))
  expect_false(crew$store$exists_output(worker$name))
})
