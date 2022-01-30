test_that("empty worker", {
  expect_silent(class_worker$new()$validate())
})

test_that("nonempty worker", {
  expect_silent(class_worker$new(name = "x")$validate())
})

test_that("invalid worker", {
  expect_error(class_worker$new(name = NULL)$validate(), class = "crew_error")
})
