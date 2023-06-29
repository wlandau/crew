crew_test("crew_launcher_local() can run a task on a worker", {
  skip_on_cran()
  skip_on_os("windows")
  client <- crew_client(
    host = "127.0.0.1",
    workers = 4L,
    tls_enable = FALSE
  )
  launcher <- crew_launcher_local(name = client$name, seconds_idle = 360)
  on.exit({
    client$terminate()
    launcher$terminate()
    rm(client)
    rm(launcher)
    gc()
    crew_test_sleep()
  })
  expect_silent(launcher$validate())
  client$start()
  launcher$start()
  expect_equal(nrow(launcher$workers), 4L)
  expect_s3_class(launcher$workers$handle[[2L]], "crew_null")
  expect_equal(launcher$workers$launches, rep(0L, 4L))
  launcher$launch(index = 2L)
  log <- client$summary()
  socket <- log$socket[2L]
  expect_s3_class(launcher$workers$handle[[2L]], "process")
  expect_silent(launcher$validate())
  crew::crew_retry(
    ~launcher$workers$handle[[2L]]$is_alive(),
    seconds_interval = 0.1,
    seconds_timeout = 5
  )
  crew::crew_retry(
    ~{
      daemons <- rlang::duplicate(
        x = mirai::daemons(.compute = client$name)$daemons,
        shallow = FALSE
      )
      if (is.null(nrow(daemons))) {
        return(FALSE)
      }
      status <- unname(daemons[, "online", drop = TRUE])[2L]
      length(status) == 1L && status > 0L
    },
    seconds_interval = 0.5,
    seconds_timeout = 10
  )
  expect_equal(launcher$workers$launches, c(0L, 1L, 0L, 0L))
  m <- mirai::mirai(ps::ps_pid(), .compute = client$name)
  crew_retry(
    ~!anyNA(m$data),
    seconds_interval = 0.5,
    seconds_timeout = 10
  )
  expect_equal(m$data, launcher$workers$handle[[2L]]$get_pid())
  client$terminate()
  tryCatch(
    crew::crew_retry(
      ~{
        handle <- launcher$workers$handle[[2L]]
        is_crew_null(handle) || !handle$is_alive()
      },
      seconds_interval = 0.1,
      seconds_timeout = 5
    ),
    crew_expire = function(condition) {
      launcher$workers$handle[[2L]]$kill()
    }
  )
})

crew_test("crew_launcher_local() can run a task and time out a worker", {
  skip_on_cran()
  skip_on_os("windows")
  client <- crew_client(
    host = "127.0.0.1",
    workers = 1L
  )
  launcher <- crew_launcher_local(
    name = client$name,
    tasks_max = 1L,
    seconds_idle = 360
  )
  on.exit({
    client$terminate()
    launcher$terminate()
    rm(client)
    rm(launcher)
    gc()
    crew_test_sleep()
  })
  client$start()
  expect_silent(launcher$validate())
  launcher$start()
  launcher$launch(index = 1L)
  crew::crew_retry(
    ~{
      handle <- launcher$workers$handle[[1]]
      !is_crew_null(handle) && handle$is_alive()
    },
    seconds_interval = 0.1,
    seconds_timeout = 5
  )
  m <- mirai::mirai(ps::ps_pid(), .compute = client$name)
  crew::crew_retry(
    ~!anyNA(m$data),
    seconds_interval = 0.1,
    seconds_timeout = 5
  )
  expect_equal(m$data, launcher$workers$handle[[1]]$get_pid())
  crew::crew_retry(
    ~{
      handle <- launcher$workers$handle[[1]]
      is_crew_null(handle) || !handle$is_alive()
    },
    seconds_interval = 0.1,
    seconds_timeout = 5
  )
  crew::crew_retry(
    ~{
      daemons <- rlang::duplicate(
        x = mirai::daemons(.compute = client$name)$daemons,
        shallow = FALSE
      )
      status <- unname(daemons[, "online", drop = TRUE])
      length(status) != 1L || status < 1L
    },
    seconds_interval = 0.5,
    seconds_timeout = 10
  )
})

crew_test("crew_launcher_local() can run a task and end a worker", {
  skip_on_cran()
  skip_on_os("windows")
  client <- crew_client(
    host = "127.0.0.1",
    workers = 1L
  )
  launcher <- crew_launcher_local(
    name = client$name,
    tasks_max = 1L,
    seconds_idle = 360
  )
  on.exit({
    client$terminate()
    launcher$terminate()
    rm(client)
    rm(launcher)
    gc()
    crew_test_sleep()
  })
  client$start()
  socket <- rownames(client$daemons)
  launcher$start()
  launcher$launch(index = 1L)
  crew::crew_retry(
    ~{
      daemons <- rlang::duplicate(
        x = mirai::daemons(.compute = client$name)$daemons,
        shallow = FALSE
      )
      status <- unname(daemons[, "online", drop = TRUE])
      length(status) == 1L && status > 0L
    },
    seconds_interval = 0.5,
    seconds_timeout = 10
  )
  crew::crew_retry(
    ~{
      handle <- launcher$workers$handle[[1]]
      !is_crew_null(handle) && handle$is_alive()
    },
    seconds_interval = 0.1,
    seconds_timeout = 5
  )
  expect_silent(launcher$terminate())
  crew::crew_retry(
    ~{
      daemons <- rlang::duplicate(
        x = mirai::daemons(.compute = client$name)$daemons,
        shallow = FALSE
      )
      status <- unname(daemons[, "online", drop = TRUE])
      length(status) != 1L || status < 1L
    },
    seconds_interval = 0.5,
    seconds_timeout = 10
  )
  crew::crew_retry(
    ~{
      handle <- launcher$workers$handle[[1]]
      is_crew_null(handle) || !handle$is_alive()
    },
    seconds_interval = 0.1,
    seconds_timeout = 5
  )
})
