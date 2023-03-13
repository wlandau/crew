crew_test("crew session", {
  port <- free_port()
  for (index in seq_len(3L)) {
    expect_equal(parallelly::freePort(port, default = NA_integer_), port)
    expect_null(crew_session_port())
    expect_null(crew_session_envir$port)
    expect_null(crew_session_envir$connection)
    crew_session_open(port = port)
    expect_equal(crew_session_port(), port)
    expect_equal(crew_session_envir$port, port)
    expect_equal(crew_session_envir$connection$state, "opened")
    crew_session_close()
    expect_null(crew_session_port())
    expect_null(crew_session_envir$port)
    expect_null(crew_session_envir$connection)
  }
})
