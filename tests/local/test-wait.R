crew_test("single controller, no tasks", {
  for (group in c(FALSE, TRUE)) {
    y <- crew_controller_local()
    x <- if_any(group, crew_controller_group(y), y)
    on.exit({
      x$terminate()
      crew_test_sleep()
    })
    x$start()
    time <- system.time(
      expect_true(
        x$wait(
          mode = "all",
          seconds_interval = 5,
          seconds_timeout = Inf
        )
      )
    )["elapsed"]
    expect_true(time < 1)
    time <- system.time(
      expect_false(
        x$wait(
          mode = "one",
          seconds_interval = 5,
          seconds_timeout = Inf
        )
      )
    )["elapsed"]
    expect_true(time < 1)
    x$terminate()
    crew_test_sleep()
  }
})

crew_test("single controller, all tasks already done", {
  for (group in c(FALSE, TRUE)) {
    y <- crew_controller_local()
    x <- if_any(group, crew_controller_group(y), y)
    on.exit({
      x$terminate()
      crew_test_sleep()
    })
    x$start()
    x$push(TRUE)
    x$wait()
    for (mode in c("all", "one")) {
      time <- system.time(
        expect_true(
          x$wait(
            mode = mode,
            seconds_interval = 0.5,
            seconds_timeout = Inf
          )
        )
      )["elapsed"]
      expect_true(time < 1)
    }
    x$terminate()
    crew_test_sleep()
  }
})

crew_test("single controller, one long task, time out", {
  for (group in c(FALSE, TRUE)) {
    y <- crew_controller_local()
    x <- if_any(group, crew_controller_group(y), y)
    on.exit({
      x$terminate()
      crew_test_sleep()
    })
    x$start()
    x$push(Sys.sleep(10))
    for (mode in c("all", "one")) {
      time <- system.time(
        expect_false(
          x$wait(
            mode = mode,
            seconds_interval = 0.1,
            seconds_timeout = 0.2
          )
        )
      )["elapsed"]
      expect_true(time < 1)
    }
    x$terminate()
    crew_test_sleep()
  }
})

crew_test("single controller, one long task, wait all, full wait", {
  for (group in c(FALSE, TRUE)) {
    for (mode in c("all", "one")) {
      y <- crew_controller_local()
      x <- if_any(group, crew_controller_group(y), y)
      on.exit({
        x$terminate()
        crew_test_sleep()
      })
      x$start()
      x$push(Sys.sleep(5))
      time <- system.time(
        expect_true(
          x$wait(
            mode = mode,
            seconds_interval = 0.5,
            seconds_timeout = Inf
          )
        )
      )["elapsed"]
      expect_true(time > 4)
      x$terminate()
      crew_test_sleep()
    }
  }
})
