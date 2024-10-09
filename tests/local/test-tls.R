crew_test("mirai client can start using custom credentials", {
  skip_on_os("windows")
  on.exit(unlink(c("fd.key", "fd.csr", "fd.crt")), add = TRUE)
  system(
    paste(
      "openssl genpkey -out fd.key -algorithm RSA -outform PEM",
      "-pkeyopt rsa_keygen_bits:2048 -des3 -pass pass:crew"
    ),
    ignore.stdout = TRUE,
    ignore.stderr = TRUE
  )
  system(
    paste(
      "openssl req -new -key fd.key -out fd.csr",
      "-subj \"/CN=127.0.0.1\" -passin pass:crew"
    ),
    ignore.stdout = TRUE,
    ignore.stderr = TRUE
  )
  system(
    paste(
      "openssl x509 -req -days 365 -in fd.csr -signkey",
      "fd.key -out fd.crt -passin pass:crew"
    ),
    ignore.stdout = TRUE,
    ignore.stderr = TRUE
  )
  on.exit(mirai::daemons(n = 0L, .compute = "name"), add = TRUE)
  expect_silent(
    tmp <- mirai::daemons(
      n = 1L,
      url = "wss://127.0.0.1:0",
      dispatcher = "process",
      seed = NULL,
      tls = c(
        paste(readLines("fd.crt"), collapse = "\n"),
        paste(readLines("fd.key"), collapse = "\n")
      ),
      pass = "crew",
      token = TRUE,
      .compute = "name"
    )
  )
  mirai::daemons(n = 0L, .compute = "name")
})

crew_test("crew_tls() custom works on real credentials", {
  skip_on_os("windows")
  on.exit(unlink(c("fd.key", "fd.csr", "fd.crt")), add = TRUE)
  system(
    paste(
      "openssl genpkey -out fd.key -algorithm RSA",
      "-outform PEM -pkeyopt rsa_keygen_bits:2048"
    ),
    ignore.stdout = TRUE,
    ignore.stderr = TRUE
  )
  system(
    "openssl req -new -key fd.key -out fd.csr -subj \"/CN=127.0.0.1\"",
    ignore.stdout = TRUE,
    ignore.stderr = TRUE
  )
  system(
    "openssl x509 -req -days 365 -in fd.csr -signkey fd.key -out fd.crt",
    ignore.stdout = TRUE,
    ignore.stderr = TRUE
  )
  tls <- crew_tls(mode = "custom", key = "fd.key", certificates = "fd.crt")
  expect_silent(tls$validate())
  x <- crew_controller_local(tls = tls)
  x$start()
  x$push("57")
  x$wait()
  expect_equal(x$pop()$result[[1L]], "57")
  x$terminate()
})

crew_test("crew_tls() custom works on real credentials with custom password", {
  skip_on_os("windows")
  on.exit(unlink(c("fd.key", "fd.csr", "fd.crt")), add = TRUE)
  system(
    paste(
      "openssl genpkey -out fd.key -algorithm RSA -outform PEM",
      "-pkeyopt rsa_keygen_bits:2048 -des3 -pass pass:crew"
    ),
    ignore.stdout = TRUE,
    ignore.stderr = TRUE
  )
  system(
    paste(
      "openssl req -new -key fd.key -out fd.csr",
      "-subj \"/CN=127.0.0.1\" -passin pass:crew"
    ),
    ignore.stdout = TRUE,
    ignore.stderr = TRUE
  )
  system(
    paste(
      "openssl x509 -req -days 365 -in fd.csr -signkey",
      "fd.key -out fd.crt -passin pass:crew"
    ),
    ignore.stdout = TRUE,
    ignore.stderr = TRUE
  )
  tls <- crew_tls(
    mode = "custom",
    key = "fd.key",
    password = "crew",
    certificates = "fd.crt"
  )
  expect_equal(tls$mode, "custom")
  expect_equal(tls$key, "fd.key")
  expect_equal(tls$certificates, "fd.crt")
  expect_equal(tls$password, "crew")
  expect_silent(tls$validate())
  x <- crew_controller_local(tls = tls)
  on.exit(x$terminate(), add = TRUE)
  utils::capture.output(suppressMessages(x$start()))
  x$push("57")
  x$wait()
  expect_equal(x$pop()$result[[1L]], "57")
  x$terminate()
})

crew_test("crew_tls() automatic", {
  x <- crew_controller_local(tls = crew_tls(mode = "automatic"))
  on.exit(x$terminate(), add = TRUE)
  utils::capture.output(suppressMessages(x$start()))
  x$push("57")
  x$wait()
  expect_equal(x$pop()$result[[1L]], "57")
  x$terminate()
})
