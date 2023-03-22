crew_test("abstract launcher class", {
  expect_silent(crew_launcher()$validate)
  expect_crew_error(crew_launcher(async_dial = -1)$validate())
})

crew_test("default terminate_launcher() method", {
  launcher <- crew_class_launcher$new(
    name = "my_launcher_name",
    seconds_launch = 1,
    seconds_idle = 2,
    seconds_wall = 3,
    seconds_exit = 4,
    tasks_max = 7,
    tasks_timers = 8,
    async_dial = FALSE,
    cleanup = TRUE
  )
  expect_null(launcher$terminate_worker())
})

crew_test("launcher settings", {
  launcher <- crew_class_launcher$new(
    name = "my_launcher_name",
    seconds_launch = 1,
    seconds_idle = 2,
    seconds_wall = 3,
    seconds_exit = 4,
    tasks_max = 7,
    tasks_timers = 8,
    async_dial = FALSE,
    cleanup = TRUE
  )
  expect_equal(launcher$name, "my_launcher_name")
  socket <- "ws://127.0.0.1:5000"
  settings <- launcher$settings(socket = socket)
  expect_equal(settings$url, socket)
  expect_equal(settings$asyncdial, FALSE)
  expect_equal(settings$maxtasks, 7)
  expect_equal(settings$idletime, 2000)
  expect_equal(settings$walltime, 3000)
  expect_equal(settings$timerstart, 8)
  expect_equal(settings$exitlinger, 4000)
  expect_true(settings$cleanup)
})

crew_test("launcher call", {
  skip_on_cran()
  skip_on_os("windows")
  launcher <- crew_class_launcher$new(
    name = "my_launcher_name",
    seconds_launch = 1,
    seconds_idle = 0.001,
    seconds_wall = 3,
    seconds_exit = 4,
    tasks_max = 7,
    tasks_timers = 8,
    async_dial = FALSE,
    cleanup = TRUE
  )
  out <- launcher$call(
    socket = "ws://127.0.0.1:90000000",
    host = "127.0.0.1",
    port = "90000000",
    token = "my_token",
    name = "my_name"
  )
  expect_true(is.character(out))
  expect_true(!anyNA(out))
  expect_equal(length(out), 1L)
  expect_true(all(nzchar(out)))
  expect_true(grepl(pattern = "^crew::crew_worker\\(", x = out))
  message <- tryCatch(eval(parse(text = out)), error = conditionMessage)
  expect_equal(message, "15 | Address invalid")
})

crew_test("launcher populate()", {
  skip_on_cran()
  launcher <- crew_class_launcher$new()
  workers <- launcher$workers
  expect_equal(dim(workers), c(0, 6))
  expect_equal(
    colnames(workers),
    c("socket", "launches", "start", "token", "listener", "handle")
  )
  expect_equal(workers$socket, character(0L))
  expect_equal(workers$launches, integer(0L))
  expect_equal(workers$start, numeric(0L))
  expect_equal(workers$token, character(0L))
  expect_equal(workers$handle, list())
  launcher$populate(sockets = paste0("ws://127.0.0.1:5000/", seq_len(2)))
  workers <- launcher$workers
  expect_equal(workers$socket, paste0("ws://127.0.0.1:5000/", seq_len(2)))
  expect_equal(workers$start, c(NA_real_, NA_real_))
  expect_equal(workers$token, c(NA_character_, NA_character_))
  expect_equal(workers$listener, list(crew_null, crew_null))
  expect_equal(workers$handle, list(crew_null, crew_null))
})

crew_test("launcher active()", {
  skip_on_cran()
  launcher <- crew_class_launcher$new(seconds_launch = 1)
  port_mirai <- free_port()
  sockets <- sprintf("ws://127.0.0.1:%s/%s", port_mirai, seq_len(9L))
  expect_equal(length(sockets), 9L)
  launcher$populate(sockets = sockets)
  launcher$workers$start <- rep(c(NA_real_, -Inf, Inf), times = 3L)
  launcher$workers$token <- replicate(9L, random_name(), simplify = TRUE)
  port_nanonext <- free_port()
  dialers <- list()
  for (index in seq_len(9L)) {
    token <- launcher$workers$token[index]
    listener <- connection_listen(
      host = local_ip(),
      port = port_nanonext,
      token = token
    )
    launcher$workers$listener[[index]] <- listener
    if (index > 3L) {
      dialer <- connection_dial(
        host = local_ip(),
        port = port_nanonext,
        token = token
      )
      dialers[[length(dialers) + 1L]] <- dialer
      Sys.sleep(0.1)
      crew_wait(
        ~dialer_discovered(listener),
        seconds_interval = 0.001,
        seconds_timeout = 5
      )
    }
    if (index > 6L) {
      close(dialer)
      connection_wait_closed(dialer)
    }
  }
  active <- launcher$active()
  crew_wait(
    ~identical(
      sort(as.character(active)),
      sort(sprintf("ws://127.0.0.1:%s/%s", port_mirai, c(3L, 4L, 5L, 6L)))
    ),
    seconds_interval = 0.001,
    seconds_timeout = 5
  )
  for (dialer in dialers) {
    if (connection_opened(dialer)) {
      close(dialer)
    }
  }
  listeners <- launcher$workers$listener
  walk(listeners, connection_wait_opened)
  launcher$terminate()
  walk(listeners, connection_wait_closed)
})
