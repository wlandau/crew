crew_test("crew_launcher_local() can run a task on a worker", {
  skip_on_cran()
  skip_on_os("windows")
  router <- crew_router(workers = 4L)
  launcher <- crew_launcher_local(seconds_idle = 360)
  on.exit({
    router$terminate()
    launcher$terminate()
    rm(router)
    rm(launcher)
    gc()
    crew_test_sleep()
  })
  expect_silent(launcher$validate())
  router$start()
  launcher$start(workers = 4L)
  expect_equal(nrow(launcher$workers), 4L)
  expect_s3_class(launcher$workers$handle[[2L]], "crew_null")
  socket <- rownames(router$daemons)[2L]
  expect_equal(launcher$workers$launches, rep(0L, 4L))
  launcher$launch(index = 2L, socket = socket)
  expect_s3_class(launcher$workers$handle[[2L]], "process")
  expect_silent(launcher$validate())
  crew::crew_retry(
    ~launcher$workers$handle[[2L]]$is_alive(),
    seconds_interval = 0.001,
    seconds_timeout = 5
  )
  crew::crew_retry(
    ~{
      daemons <- rlang::duplicate(
        x = mirai::daemons(.compute = router$name)$daemons,
        shallow = FALSE
      )
      if (is.null(nrow(daemons))) {
        return(FALSE)
      }
      status <- unname(daemons[, "online", drop = TRUE])[2L]
      length(status) == 1L && status > 0L
    },
    seconds_interval = 0.001,
    seconds_timeout = 5
  )
  expect_equal(launcher$workers$launches, c(0L, 1L, 0L, 0L))
  m <- mirai::mirai(ps::ps_pid(), .compute = router$name)
  crew_retry(
    ~!anyNA(m$data),
    seconds_interval = 0.001,
    seconds_timeout = 5
  )
  expect_equal(m$data, launcher$workers$handle[[2L]]$get_pid())
  router$terminate()
  tryCatch(
    crew::crew_retry(
      ~{
        handle <- launcher$workers$handle[[2L]]
        is_crew_null(handle) || !handle$is_alive()
      },
      seconds_interval = 0.001,
      seconds_timeout = 5
    ),
    crew_expire = function(condition) {
      launcher$workers$handle[[2L]]$kill()
    }
  )
})

crew_test("crew_launcher_local() okay to not have sockets to launch", {
  skip_on_cran()
  skip_on_os("windows")
  launcher <- crew_launcher_local(seconds_idle = 360)
  # start launcher
  launcher$start()
  expect_true(is_crew_null(launcher$workers$handle[[1]]))
  expect_equal(launcher$workers$socket, NA_character_)
  expect_equal(launcher$workers$start, NA_real_)
  expect_equal(launcher$workers$launches, 0L)
  # launch with empty character vector
  expect_silent(launcher$launch(index = 1L, socket = character(0L)))
  expect_true(is_crew_null(launcher$workers$handle[[1]]))
  expect_equal(launcher$workers$socket, NA_character_)
  expect_equal(launcher$workers$start, NA_real_)
  expect_equal(launcher$workers$launches, 0L)
  # launch null
  expect_silent(launcher$launch(index = 1L, socket = NULL))
  expect_true(is_crew_null(launcher$workers$handle[[1]]))
  expect_equal(launcher$workers$socket, NA_character_)
  expect_equal(launcher$workers$start, NA_real_)
  expect_equal(launcher$workers$launches, 0L)
})

crew_test("crew_launcher_local() can run a task and time out a worker", {
  skip_on_cran()
  skip_on_os("windows")
  router <- crew_router(workers = 1L)
  launcher <- crew_launcher_local(tasks_max = 1L, seconds_idle = 360)
  on.exit({
    router$terminate()
    launcher$terminate()
    rm(router)
    rm(launcher)
    gc()
    crew_test_sleep()
  })
  router$start()
  expect_silent(launcher$validate())
  socket <- rownames(router$daemons)
  launcher$start(workers = 1L)
  launcher$launch(index = 1L, socket = socket)
  crew::crew_retry(
    ~{
      handle <- launcher$workers$handle[[1]]
      !is_crew_null(handle) && handle$is_alive()
    },
    seconds_interval = 0.001,
    seconds_timeout = 5
  )
  m <- mirai::mirai(ps::ps_pid(), .compute = router$name)
  crew::crew_retry(
    ~!anyNA(m$data),
    seconds_interval = 0.001,
    seconds_timeout = 5
  )
  expect_equal(m$data, launcher$workers$handle[[1]]$get_pid())
  crew::crew_retry(
    ~{
      handle <- launcher$workers$handle[[1]]
      is_crew_null(handle) || !handle$is_alive()
    },
    seconds_interval = 0.001,
    seconds_timeout = 5
  )
  crew::crew_retry(
    ~{
      daemons <- rlang::duplicate(
        x = mirai::daemons(.compute = router$name)$daemons,
        shallow = FALSE
      )
      status <- unname(daemons[, "online", drop = TRUE])
      length(status) != 1L || status < 1L
    },
    seconds_interval = 0.001,
    seconds_timeout = 5
  )
})

crew_test("crew_launcher_local() can run a task and end a worker", {
  skip_on_cran()
  skip_on_os("windows")
  router <- crew_router(workers = 1L)
  launcher <- crew_launcher_local(tasks_max = 1L, seconds_idle = 360)
  on.exit({
    router$terminate()
    launcher$terminate()
    rm(router)
    rm(launcher)
    gc()
    crew_test_sleep()
  })
  router$start()
  socket <- rownames(router$daemons)
  launcher$start(workers = 1L)
  launcher$launch(index = 1L, socket = socket)
  crew::crew_retry(
    ~{
      daemons <- rlang::duplicate(
        x = mirai::daemons(.compute = router$name)$daemons,
        shallow = FALSE
      )
      status <- unname(daemons[, "online", drop = TRUE])
      length(status) == 1L && status > 0L
    },
    seconds_interval = 0.001,
    seconds_timeout = 5
  )
  crew::crew_retry(
    ~{
      handle <- launcher$workers$handle[[1]]
      !is_crew_null(handle) && handle$is_alive()
    },
    seconds_interval = 0.001,
    seconds_timeout = 5
  )
  expect_silent(launcher$terminate())
  crew::crew_retry(
    ~{
      daemons <- rlang::duplicate(
        x = mirai::daemons(.compute = router$name)$daemons,
        shallow = FALSE
      )
      status <- unname(daemons[, "online", drop = TRUE])
      length(status) != 1L || status < 1L
    },
    seconds_interval = 0.001,
    seconds_timeout = 5
  )
  crew::crew_retry(
    ~{
      handle <- launcher$workers$handle[[1]]
      is_crew_null(handle) || !handle$is_alive()
    },
    seconds_interval = 0.001,
    seconds_timeout = 5
  )
})
