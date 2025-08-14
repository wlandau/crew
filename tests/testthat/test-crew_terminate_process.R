crew_test("crew_terminate_process()", {
  skip_on_cran()
  process <- processx::process$new("sleep", "600")
  crew_retry(
    ~ process$is_alive(),
    seconds_interval = 0.1,
    seconds_timeout = 30
  )
  expect_true(process$is_alive())
  crew_terminate_process(pid = process$get_pid())
  crew_retry(
    ~ !process$is_alive(),
    seconds_interval = 0.1,
    seconds_timeout = 30
  )
  expect_false(process$is_alive())
})
