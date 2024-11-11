test_that("crashes_error allows idle workers to exit", {
  x <- crew_controller_local(crashes_error = 2L, seconds_idle = 0.25)
  on.exit(x$terminate())
  x$start()
  for (index in seq_len(10L)) {
    x$launch()
    Sys.sleep(5)
  }
  expect_equal(x$launcher$crashes(index = 1L), 0L)
  expect_equal(index, 10L)
})

test_that("crashes_error detects when there are too many crashes", {
  x <- crew_controller_local(
    name = "name",
    workers = 2L,
    crashes_error = 2L,
    tasks_max = 1L,
    seconds_idle = 1
  )
  on.exit(x$terminate())
  x$start()
  x$push(Sys.sleep(4), scale = TRUE)
  Sys.sleep(5)
  x$push(Sys.sleep(1e3), scale = TRUE)
  x$wait(mode = "one")
  for (index in seq_len(10L)) {
    Sys.sleep(5)
    if (index == 3L) {
      expect_error(x$scale(), class = "crew_error")
      break
    } else {
      expect_silent(x$scale())
    }
    expect_equal(x$launcher$crashes(index = 1L), 0L)
    expect_equal(x$launcher$crashes(index = 2L), index - 1L)
    Sys.sleep(5)
    client_summary <- x$client$summary()
    expect_equal(client_summary$online, c(FALSE, TRUE))
    expect_equal(client_summary$instances, c(1L, index))
    expect_equal(client_summary$assigned, c(1L, 1L))
    expect_equal(client_summary$complete, c(1L, 0L))
    x$launcher$workers$handle[[2L]]$kill()
  }
  expect_equal(index, 3L)
})

# nolint start
# TODO: re-enable the test below if/when mirai::stop_mirai() decrements the
# assigned counter: https://github.com/shikokuchuo/mirai/issues/163
# test_that("crash counter resets when a worker succeeds", {
#   x <- crew_controller_local(name = "name", crashes_error = 3L)
#   x$start()
#   x$push(Sys.sleep(1e3), scale = FALSE)
#   for (index in seq_len(2L)) {
#     Sys.sleep(5)
#     x$scale()
#     Sys.sleep(5)
#     x$launcher$workers$handle[[1L]]$kill()
#   }
#   x$cancel(all = TRUE)
#   expect_equal(x$launcher$workers$crashes, 1L)
#   x$push(TRUE)
#   x$wait()
#   expect_gt(x$launcher$workers$crashes, 0L)
#   x$push(Sys.sleep(1e3), scale = FALSE)
#   for (index in seq_len(2L)) {
#     Sys.sleep(5)
#     x$scale()
#     Sys.sleep(5)
#     x$launcher$workers$handle[[1L]]$kill()
#   }
#   expect_gt(x$launcher$workers$crashes, 1L)
#   testthat::expect_equal(index, 3L)
#   x$terminate()
# })
# nolint end
