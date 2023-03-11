crew_test("crew port setting", {
  port <- free_port()
  for (index in seq_len(3L)) {
    expect_equal(parallelly::freePort(port, default = NA_integer_), port)
    expect_null(crew_port_get())
    expect_null(crew_port_envir$port)
    expect_null(crew_port_envir$connection)
    crew_port_set(port = port)
    expect_equal(crew_port_get(), port)
    expect_equal(crew_port_envir$port, port)
    expect_equal(crew_port_envir$connection$state, "opened")
    crew_port_unset()
    expect_null(crew_port_get())
    expect_null(crew_port_envir$port)
    expect_null(crew_port_envir$connection)
  }
})
