crew_test("mirai_status()", {
  skip_on_cran()
  skip_on_os("windows")
  out <- mirai_status(
    profile = "none",
    seconds_interval = 1L,
    seconds_timeout = 15L
  )
  expect_true(is.list(out))
})

crew_test("mirai_status_error()", {
  skip_on_cran()
  expect_crew_error(mirai_status_error(status = 5L, profile = "default"))
})

crew_test("mirai_resolved()", {
  skip_on_cran()
  skip_on_os("windows")
  expect_true(mirai_resolved(TRUE))
  task <- mirai::mirai(TRUE)
  mirai::call_mirai_(task)
  expect_true(mirai_resolved(task))
  ask <- mirai::mirai(stop("abc"))
  mirai::call_mirai_(task)
  expect_true(mirai_resolved(task))
  task <- mirai::mirai(Sys.sleep(1))
  expect_false(mirai_resolved(task))
  mirai::call_mirai_(task)
})

crew_test("mirai_resolve()", {
  skip_on_cran()
  skip_on_os("windows")
  expect_equal(mirai_resolve(list("abc")), list("abc"))
  expect_equal(
    mirai_resolve(mirai::mirai(list(a = 1L, b = 2L))),
    list(a = 1L, b = 2L)
  )
})

crew_test("mirai_wait_terminate()", {
  skip_on_cran()
  skip_on_os("windows")
  tasks <- replicate(2L, mirai::mirai(TRUE), simplify = FALSE)
  expect_silent(mirai_wait_terminate(tasks))
  expect_crew_error(
    mirai_wait_terminate(
      list(
        mirai::mirai(TRUE),
        mirai::mirai({
          Sys.sleep(1)
          stop("abc")
        })
      )
    )
  )
})

crew_test("mirai_assert_launch()", {
  skip_on_cran()
  skip_on_os("windows")
  expect_silent(mirai_assert_launch("xyz"))
  task <- mirai::mirai(TRUE)
  mirai::call_mirai_(task)
  expect_silent(mirai_assert_launch(task))
  task <- mirai::mirai(stop("123"))
  mirai::call_mirai_(task)
  expect_crew_error(mirai_assert_launch(task))
})

crew_test("mirai_assert_terminate()", {
  skip_on_cran()
  skip_on_os("windows")
  expect_silent(mirai_assert_terminate("xyz"))
  task <- mirai::mirai(TRUE)
  mirai::call_mirai_(task)
  expect_silent(mirai_assert_terminate(task))
  task <- mirai::mirai(stop("123"))
  mirai::call_mirai_(task)
  expect_crew_error(mirai_assert_terminate(task))
})
