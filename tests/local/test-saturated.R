crew_test("crew_controller_local() saturated", {
  skip_on_cran()
  skip_on_os("windows")
  x <- crew_controller_local(
    workers = 4L,
    seconds_idle = 360,
    seconds_interval = 0.1
  )
  on.exit({
    x$terminate()
    rm(x)
    gc()
    crew_test_sleep()
  })
  x$start()
  expect_false(x$saturated())
  x$push(Sys.sleep(1))
  expect_false(x$saturated())
  x$push(Sys.sleep(1))
  expect_false(x$saturated())
  x$push(Sys.sleep(1))
  expect_false(x$saturated())
  x$push(Sys.sleep(1))
  expect_true(x$saturated())
  x$push(Sys.sleep(1))
  expect_true(x$saturated())
  x$wait(mode = "all")
  x$terminate()
})

crew_test("crew_controller_group() saturate a", {
  skip_on_cran()
  skip_on_os("windows")
  a <- crew_controller_local(
    name = "a",
    workers = 4L,
    seconds_idle = 360,
    seconds_interval = 0.1
  )
  b <- crew_controller_local(
    name = "b",
    workers = 4L,
    seconds_idle = 360,
    seconds_interval = 0.1
  )
  x <- crew_controller_group(a, b)
  on.exit({
    x$terminate()
    rm(x)
    gc()
    crew_test_sleep()
  })
  x$start()
  expect_false(x$saturated())
  expect_false(x$saturated(controller = "a"))
  expect_false(x$saturated(controller = "b"))
  x$push(Sys.sleep(1), controller = "a")
  expect_false(x$saturated())
  expect_false(x$saturated(controller = "a"))
  expect_false(x$saturated(controller = "b"))
  x$push(Sys.sleep(1), controller = "a")
  expect_false(x$saturated())
  expect_false(x$saturated(controller = "a"))
  expect_false(x$saturated(controller = "b"))
  x$push(Sys.sleep(1), controller = "a")
  expect_false(x$saturated())
  expect_false(x$saturated(controller = "a"))
  expect_false(x$saturated(controller = "b"))
  x$push(Sys.sleep(1), controller = "a")
  expect_true(x$saturated())
  expect_true(x$saturated(controller = "a"))
  expect_false(x$saturated(controller = "b"))
  x$push(Sys.sleep(1), controller = "a")
  expect_true(x$saturated())
  expect_true(x$saturated(controller = "a"))
  expect_false(x$saturated(controller = "b"))
  x$wait(mode = "all")
  x$terminate()
})

crew_test("crew_controller_group() saturate b", {
  skip_on_cran()
  skip_on_os("windows")
  a <- crew_controller_local(
    name = "a",
    workers = 4L,
    seconds_idle = 360,
    seconds_interval = 0.1
  )
  b <- crew_controller_local(
    name = "b",
    workers = 4L,
    seconds_idle = 360,
    seconds_interval = 0.1
  )
  x <- crew_controller_group(a, b)
  on.exit({
    x$terminate()
    rm(x)
    gc()
    crew_test_sleep()
  })
  x$start()
  expect_false(x$saturated())
  expect_false(x$saturated(controller = "a"))
  expect_false(x$saturated(controller = "b"))
  x$push(Sys.sleep(1), controller = "b")
  expect_false(x$saturated())
  expect_false(x$saturated(controller = "a"))
  expect_false(x$saturated(controller = "b"))
  x$push(Sys.sleep(1), controller = "b")
  expect_false(x$saturated())
  expect_false(x$saturated(controller = "a"))
  expect_false(x$saturated(controller = "b"))
  x$push(Sys.sleep(1), controller = "b")
  expect_false(x$saturated())
  expect_false(x$saturated(controller = "a"))
  expect_false(x$saturated(controller = "b"))
  x$push(Sys.sleep(1), controller = "b")
  expect_false(x$saturated())
  expect_false(x$saturated(controller = "a"))
  expect_true(x$saturated(controller = "b"))
  x$push(Sys.sleep(1), controller = "b")
  expect_false(x$saturated())
  expect_false(x$saturated(controller = "a"))
  expect_true(x$saturated(controller = "b"))
  x$wait(mode = "all")
  x$terminate()
})
