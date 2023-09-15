test_that("crew_tls() none", {
  expect_silent(crew_tls(mode = "none")$validate())
})

test_that("crew_tls() automatic", {
  expect_silent(crew_tls(mode = "automatic")$validate())
})

test_that("crew_tls() custom with no files", {
  expect_crew_error(crew_tls(mode = "custom")$validate())
})

test_that("crew_tls() bad mode", {
  expect_crew_error(crew_tls(mode = "nope")$validate())
})

test_that("crew_tls_assert_key()", {
  temp <- tempfile()
  on.exit(unlink(temp))
  expect_crew_error(crew_tls_assert_key(temp))
  file.create(temp)
  expect_crew_error(crew_tls_assert_key(temp))
  writeLines(c("abc", "123"), temp)
  expect_crew_error(crew_tls_assert_key(temp))
  writeLines(
    c(
      "-----BEGIN PRIVATE KEY-----",
      "lines",
      "-----END PRIVATE KEY-----"
    ),
    temp
  )
  expect_silent(crew_tls_assert_key(temp))
  writeLines(
    c(
      "-----BEGIN ENCRYPTED PRIVATE KEY-----",
      "lines",
      "-----END ENCRYPTED PRIVATE KEY-----"
    ),
    temp
  )
  expect_silent(crew_tls_assert_key(temp))
})

test_that("crew_tls_assert_certificate()", {
  temp <- tempfile()
  on.exit(unlink(temp))
  expect_crew_error(crew_tls_assert_certificate(temp))
  file.create(temp)
  expect_crew_error(crew_tls_assert_certificate(temp))
  writeLines(c("abc", "123"), temp)
  expect_crew_error(crew_tls_assert_certificate(temp))
  writeLines(
    c(
      "-----BEGIN CERTIFICATE-----",
      "lines",
      "-----END CERTIFICATE-----"
    ),
    temp
  )
  expect_silent(crew_tls_assert_certificate(temp))
})

test_that("crew_tls() with mock credentials", {
  key <- tempfile()
  certificates <- tempfile()
  on.exit(unlink(c(key, certificates)))
  writeLines(
    c(
      "-----BEGIN PRIVATE KEY-----",
      "lines",
      "-----END PRIVATE KEY-----"
    ),
    key
  )
  writeLines(
    c(
      "-----BEGIN CERTIFICATE-----",
      "lines",
      "-----END CERTIFICATE-----"
    ),
    certificates
  )
  tls <- crew_tls(mode = "custom", key = key, certificates = certificates)
  expect_silent(tls$validate())
})
