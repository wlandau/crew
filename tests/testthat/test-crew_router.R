crew_test("crew_router() validate", {
  router <- crew_router()
  expect_silent(router$validate())
  router$name <- NULL
  expect_crew_error(router$validate())
})

crew_test("crew_router() works", {
  router <- crew_router()
  on.exit(router$terminate())
  expect_false(router$listening())
  expect_equal(router$sockets(), character(0))
  expect_equal(router$connected(), character(0))
  expect_equal(router$busy(), character(0))
  expect_equal(router$idle(), character(0))
  expect_silent(router$listen())
  expect_true(router$listening())
  socket <- router$sockets()
  expect_true(is.character(socket) && length(socket) > 0L)
  expect_true(nzchar(socket) && !anyNA(socket))
  expect_equal(length(socket), 1L)
  expect_equal(router$connected(), character(0))
  expect_equal(router$busy(), character(0))
  expect_equal(router$idle(), character(0))
  px <- callr::r_bg(
    function(socket) mirai::server(socket),
    args = list(socket = socket)
  )
  crew::crew_wait(
    ~identical(router$connected(), socket),
    timeout = 5,
    wait = 0.1
  )
  expect_equal(router$busy(), character(0))
  expect_equal(router$idle(), socket)
  m <- mirai::mirai(ps::ps_pid(), .compute = router$name)
  crew::crew_wait(~!anyNA(m$data), timeout = 5, wait = 0.1)
  expect_false(anyNA(m$data))
  expect_true(is.numeric(m$data))
  expect_true(abs(m$data - ps::ps_pid()) > 0.5)
  expect_true(router$listening())
  expect_silent(router$terminate())
  expect_false(router$listening())
  px$kill()
})

crew_test("crew_router() polling methods", {
  nodes <- rbind(
    c(0, 0, 0, 0), # a
    c(1, 0, 0, 0), # b
    c(0, 1, 0, 0), # c
    c(1, 1, 0, 0), # d
    c(0, 0, 1, 0), # e
    c(1, 0, 1, 0), # f
    c(0, 1, 1, 0), # g
    c(1, 1, 1, 0), # h
    c(0, 0, 1, 1), # i
    c(1, 0, 1, 1), # j
    c(0, 1, 1, 1), # k
    c(1, 1, 1, 1), # l
    c(0, 0, 2, 1), # m
    c(1, 0, 2, 1)  # n
  )
  rownames(nodes) <- letters[seq_len(nrow(nodes))]
  colnames(nodes) <- c(
    "status_online",
    "status_busy",
    "tasks_assigned",
    "tasks_complete"
  )
  sockets <- router_sockets(nodes)
  connected <- router_connected(nodes)
  busy <- router_busy(nodes)
  idle <- router_idle(nodes)
  expect_equal(sockets, rownames(nodes))
  expect_equal(connected, c("b", "d", "f", "h", "j", "l", "n"))
  expect_equal(busy, c("d", "f", "h", "l", "n"))
  expect_equal(idle, c("b", "j"))
})
