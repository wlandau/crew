test_that("crew_tls() none", {
  x <- crew_tls(mode = "none")
  expect_silent(x$validate())
  expect_null(x$client())
  expect_null(x$worker(name = "default"))
})

test_that("crew_tls() automatic", {
  x <- crew_tls(mode = "automatic")
  expect_silent(x$validate())
  expect_null(x$client())
  expect_null(x$worker(name = "default"))
})

test_that("crew_tls() custom with no files", {
  expect_crew_error(crew_tls(mode = "custom")$validate())
})

test_that("crew_tls() with mock files", {
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
  tls <- crew_tls(
    mode = "custom",
    key = key,
    certificates = certificates,
    validate = FALSE
  )
  expect_equal(tls$mode, "custom")
  expect_equal(tls$key, key)
  expect_equal(tls$certificates, certificates)
  expect_null(tls$password)
  expect_silent(tls$validate(test = FALSE))
  expect_equal(
    tls$client(),
    c(
      "-----BEGIN CERTIFICATE-----\nlines\n-----END CERTIFICATE-----",
      "-----BEGIN PRIVATE KEY-----\nlines\n-----END PRIVATE KEY-----"
    )
  )
  expect_equal(
    tls$worker(name = "default"),
    c(
      "-----BEGIN CERTIFICATE-----\nlines\n-----END CERTIFICATE-----",
      ""
    )
  )
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
