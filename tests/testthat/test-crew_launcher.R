crew_test("abstract launcher class", {
  expect_silent(crew_class_launcher$new()$validate)
  expect_crew_error(crew_class_launcher$new(async_dial = -1)$validate())
})

crew_test("launcher args", {
  launcher <- crew_class_launcher$new(
    seconds_launch = 1,
    seconds_idle = 2,
    seconds_wall = 3,
    seconds_exit = 4,
    seconds_poll_high = 5,
    seconds_poll_low = 6,
    tasks_max = 7,
    tasks_timers = 8,
    async_dial = FALSE
  )
  socket <- "ws://127.0.0.1:5000"
  args <- launcher$args(socket = socket)
  expect_equal(args$url, socket)
  expect_null(args$nodes)
  expect_equal(args$asyncdial, FALSE)
  expect_equal(args$maxtasks, 7)
  expect_equal(args$idletime, 2000)
  expect_equal(args$walltime, 3000)
  expect_equal(args$timerstart, 8)
  expect_equal(args$exitdelay, 4000)
  expect_equal(args$pollfreqh, 5000)
  expect_equal(args$pollfreql, 6000)
})

crew_test("launcher populate()", {
  launcher <- crew_class_launcher$new()
  workers <- launcher$workers
  expect_equal(dim(workers), c(0, 3))
  expect_equal(colnames(workers), c("socket", "start", "handle"))
  expect_equal(workers$socket, character(0))
  expect_equal(workers$start, numeric(0))
  expect_equal(workers$handle, list())
  launcher$populate(sockets = paste0("ws://127.0.0.1:5000/", seq_len(2)))
  workers <- launcher$workers
  expect_equal(workers$socket, paste0("ws://127.0.0.1:5000/", seq_len(2)))
  expect_equal(workers$start, c(-Inf, -Inf))
  expect_equal(workers$handle, list(NA, NA))
})

crew_test("launcher launching() and launched()", {
  launcher <- crew_class_launcher$new(seconds_launch = 1)
  launcher$populate(sockets = paste0("ws://127.0.0.1:5000/", seq_len(2)))
  launcher$workers$start <- c(-Inf, Inf)
  expect_equal(launcher$launched(), "ws://127.0.0.1:5000/1")
  expect_equal(launcher$launching(), "ws://127.0.0.1:5000/2")
})
