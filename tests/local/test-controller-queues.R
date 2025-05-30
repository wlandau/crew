crew_test("queue_resolved and queue_unresolved", {
  x <- crew_controller_local(workers = 3)
  on.exit(x$terminate())
  x$push(1, name = "fast_1")
  x$push(2, name = "fast_2")
  x$push(Sys.sleep(10000), name = "slow_1")
  x$push(Sys.sleep(10000), name = "slow_2")
  x$wait(seconds_timeout = 3)
  expect_equal(x$queue_resolved$list(), character(0L))
  expect_equal(
    sort(x$queue_unresolved$list()),
    sort(c("fast_1", "fast_2", "slow_1", "slow_2"))
  )
  task <- x$pop()
  expect_true(task$name %in% c("fast_1", "fast_2"))
  resolved <- x$queue_resolved$list()
  expect_equal(length(resolved), 1L)
  expect_true(resolved %in% c("fast_1", "fast_2"))
  expect_equal(
    sort(x$queue_unresolved$list()),
    sort(c("slow_1", "slow_2"))
  )
  x$push(3, name = "fast_3")
  resolved <- x$queue_resolved$list()
  expect_equal(length(resolved), 1L)
  expect_true(resolved %in% c("fast_1", "fast_2"))
  expect_equal(
    sort(x$queue_unresolved$list()),
    sort(c("fast_3", "slow_1", "slow_2"))
  )
  x$scale()
  x$wait(seconds_timeout = 3)
  # .resolve() is not called because a task was still in queue_resolved:
  task <- x$pop()
  expect_true(task$name %in% c("fast_1", "fast_2"))
  expect_equal(x$queue_resolved$list(), character(0L))
  expect_equal(
    sort(x$queue_unresolved$list()),
    sort(c("fast_3", "slow_1", "slow_2"))
  )
  # Now .resolve() is called and modifies both queues:
  task <- x$pop()
  expect_equal(task$name, "fast_3")
  expect_equal(x$queue_resolved$list(), character(0L))
  expect_equal(
    sort(x$queue_unresolved$list()),
    sort(c("slow_1", "slow_2"))
  )
  expect_null(x$pop())
  expect_equal(x$queue_resolved$list(), character(0L))
  expect_equal(
    sort(x$queue_unresolved$list()),
    sort(c("slow_1", "slow_2"))
  )
})
