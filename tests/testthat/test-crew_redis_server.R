crew_test("redis server serialize", {
  skip_redis()
  server <- crew_redis_server(
    host = "localhost",
    port = "8080",
    password = "abcde"
  )
  out <- server$serialize()
  expect_equal(out, "host=localhost&port=8080&password=abcde")
})

crew_test("redis server client, start, alive, ping, stop", {
  skip_redis()
  server <- crew_redis_server()
  expect_error(server$client(), class = "crew_error")
  expect_false(server$alive())
  server$start()
  expect_true(server$alive())
  client <- server$client()
  expect_equal(as.character(client$PING()), "PONG")
  expect_equal(as.character(server$ping()), "PONG")
  expect_silent(server$start())
  expect_silent(server$stop())
  expect_false(server$alive())
  expect_silent(server$stop())
})

crew_test("redis server ready", {
  skip_redis()
  server <- crew_redis_server()
  expect_false(server$ready())
  server$start()
  expect_true(server$ready())
  server$stop()
  expect_false(server$ready())
})

crew_test("redis server test", {
  skip_redis()
  server <- crew_redis_server()
  expect_true(server$test())
  expect_false(server$alive())
  server$start()
  expect_error(server$test(), class = "crew_error")
  server$stop()
})

crew_test("redis server validate and configured", {
  skip_redis()
  server <- crew_redis_server()
  expect_silent(server$validate())
  expect_true(server$configured())
  server$conf <- tempfile()
  unlink(server$conf)
  expect_error(server$validate(), class = "crew_error")
  expect_false(server$configured())
})

crew_test("redis_server_random_password()", {
  password <- redis_server_random_password()
  expect_true(is.character(password))
  expect_false(anyNA(password))
  expect_equal(length(password), 1L)
  expect_equal(nchar(password), 64L)
})

crew_test("redis_server_random_port()", {
  port <- redis_server_random_port(lower = 49152L, upper = 65355L)
  expect_true(is.character(port))
  expect_false(anyNA(port))
  expect_true(nzchar(port) > 0L)
  expect_equal(length(port), 1L)
  port <- as.integer(port)
  expect_true(is.integer(port))
  expect_true(is.finite(port))
  expect_true(port >= 49152L)
  expect_true(port <= 65535L)
})
