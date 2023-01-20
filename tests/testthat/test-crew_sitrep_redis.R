test_that("crew_sitrep_redis()", {
  if (redis_server_ok()) {
    suppressMessages(expect_message(crew_sitrep_redis()))
  }
})

test_that("sitrep_redis_server_not_exist()", {
  suppressMessages(
    expect_message(
      sitrep_redis_server_not_exist()
    )
  )
})

test_that("sitrep_redis_server_not_ok()", {
  suppressMessages(
    expect_message(
      sitrep_redis_server_not_ok()
    )
  )
})
