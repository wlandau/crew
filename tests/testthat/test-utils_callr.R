test_that("callr_session_ready() starting", {
  handle <- list(get_state = function() "not starting")
  expect_true(callr_session_ready(handle))
})

test_that("callr_session_ready() read NULL", {
  handle <- list(get_state = function() "starting", read = function() NULL)
  expect_false(callr_session_ready(handle))
})

test_that("callr_session_ready() read good code", {
  handle <- list(
    get_state = function() "starting",
    read = function() list(code = 201L)
  )
  expect_true(callr_session_ready(handle))
})

test_that("callr_session_ready() read bad code", {
  handle <- list(
    get_state = function() "starting",
    read = function() list(code = 202L)
  )
  expect_error(callr_session_ready(handle), class = "crew_error")
})
