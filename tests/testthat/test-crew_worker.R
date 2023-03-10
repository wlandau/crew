crew_test("crew_worker_socket_done()", {
  exp <- "ws://127.0.32.1:50010/done"
  sockets <- c(
    "ws://127.0.32.1:50010",
    "ws://127.0.32.1:50010/1",
    "ws://127.0.32.1:50010/2",
    "ws://127.0.32.1:50010/5757"
  )
  for (socket in sockets) {
    expect_equal(crew_worker_socket_done(socket), exp)
  }
})

crew_test("crew_worker_send_token()", {
  router <- crew_router()
  router$listen()
  on.exit(router$terminate())
  socket <- crew_worker_socket_done(rownames(router$nodes()))
  token <- random_name()
  crew_worker_send_token(socket = socket, token = token)
  connection <- nanonext::socket(protocol = "rep", listen = socket)
  nanonext::recv()
})



