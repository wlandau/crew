# This test assumes no other processes are running crew_worker().
crew_test("crash detection with backup controllers in a group", {
  skip_on_cran()
  skip_on_os("windows")
  c <- crew_controller_local(
    name = "c",
    workers = 1L,
    seconds_idle = 360,
    crashes_max = 2L
  )
  b <- crew_controller_local(
    name = "b",
    workers = 1L,
    seconds_idle = 360,
    crashes_max = 2L,
    backup = c
  )
  a <- crew_controller_local(
    name = "a",
    workers = 1L,
    seconds_idle = 360,
    crashes_max = 2L,
    backup = b
  )
  x <- crew_controller_group(a, b, c)
  on.exit({
    x$terminate()
    rm(x)
    rm(a)
    rm(b)
    rm(c)
    gc()
    crew_test_sleep()
  })
  expect_equal(x$crashes(name = "x"), 0L)
  expect_equal(a$crashes(name = "x"), 0L)
  expect_equal(b$crashes(name = "x"), 0L)
  expect_equal(c$crashes(name = "x"), 0L)
  crash <- function() {
    a$push(Sys.sleep(300L), name = "x")
    crew_retry(
      ~ {
        x$scale()
        isTRUE(a$client$status()["connections"] > 0L) ||
          isTRUE(b$client$status()["connections"] > 0L) ||
          isTRUE(c$client$status()["connections"] > 0L)
      },
      seconds_interval = 0.1,
      seconds_timeout = 60
    )
    processes <- ps::ps()
    processes <- processes[processes$name == "R", ]
    for (pid in processes$pid) {
      try(
        {
          handle <- ps::ps_handle(pid)
          command <- ps::ps_cmdline(handle)
          if (any(grepl("crew::crew_worker(", command, fixed = TRUE))) {
            ps::ps_kill(handle)
          }
        },
        silent = TRUE
      )
    }
    x$wait(mode = "one", seconds_timeout = 30, scale = FALSE)
    x$pop()
  }
  out <- crash()
  expect_true(tibble::is_tibble(out))
  expect_equal(out$controller, "a")
  expect_equal(x$crashes(name = "x"), 1L)
  expect_equal(a$crashes(name = "x"), 1L)
  expect_equal(b$crashes(name = "x"), 0L)
  expect_equal(c$crashes(name = "x"), 0L)
  summary <- x$summary()
  expect_equal(summary$controller, c("a", "b", "c"))
  expect_equal(summary$crash, c(1L, 0L, 0L))
  expect_equal(summary$error, c(0L, 0L, 0L))
  out <- crash()
  expect_true(tibble::is_tibble(out))
  expect_equal(out$controller, "a")
  expect_equal(x$crashes(name = "x"), 2L)
  expect_equal(a$crashes(name = "x"), 2L)
  expect_equal(b$crashes(name = "x"), 0L)
  expect_equal(c$crashes(name = "x"), 0L)
  summary <- x$summary()
  expect_equal(summary$controller, c("a", "b", "c"))
  expect_equal(summary$crash, c(2L, 0L, 0L))
  expect_equal(summary$error, c(0L, 0L, 0L))
  out <- crash()
  expect_true(tibble::is_tibble(out))
  expect_equal(out$controller, "b")
  expect_equal(x$crashes(name = "x"), 3L)
  expect_equal(a$crashes(name = "x"), 2L)
  expect_equal(b$crashes(name = "x"), 1L)
  expect_equal(c$crashes(name = "x"), 0L)
  summary <- x$summary()
  expect_equal(summary$controller, c("a", "b", "c"))
  expect_equal(summary$crash, c(2L, 1L, 0L))
  out <- crash()
  expect_true(tibble::is_tibble(out))
  expect_equal(out$controller, "b")
  expect_equal(x$crashes(name = "x"), 4L)
  expect_equal(a$crashes(name = "x"), 2L)
  expect_equal(b$crashes(name = "x"), 2L)
  expect_equal(c$crashes(name = "x"), 0L)
  summary <- x$summary()
  expect_equal(summary$controller, c("a", "b", "c"))
  expect_equal(summary$crash, c(2L, 2L, 0L))
  out <- crash()
  expect_true(tibble::is_tibble(out))
  expect_equal(out$controller, "c")
  expect_equal(x$crashes(name = "x"), 5L)
  expect_equal(a$crashes(name = "x"), 2L)
  expect_equal(b$crashes(name = "x"), 2L)
  expect_equal(c$crashes(name = "x"), 1L)
  summary <- x$summary()
  expect_equal(summary$controller, c("a", "b", "c"))
  expect_equal(summary$crash, c(2L, 2L, 1L))
  out <- crash()
  expect_true(tibble::is_tibble(out))
  expect_equal(out$controller, "c")
  expect_equal(x$crashes(name = "x"), 6L)
  expect_equal(a$crashes(name = "x"), 2L)
  expect_equal(b$crashes(name = "x"), 2L)
  expect_equal(c$crashes(name = "x"), 2L)
  summary <- x$summary()
  expect_equal(summary$controller, c("a", "b", "c"))
  expect_equal(summary$crash, c(2L, 2L, 2L))
  expect_crew_error(crash())
})
