test_that("mirai client can start using custom credentials", {
  skip_on_os("windows")
  on.exit(unlink(c("fd.key", "fd.csr", "fd.crt")))
  system(
    paste(
      "openssl genpkey -out fd.key -algorithm RSA -outform PEM",
      "-pkeyopt rsa_keygen_bits:2048 -des3 -pass pass:crew"
    )
  )
  system(
    paste(
      "openssl req -new -key fd.key -out fd.csr",
      "-subj \"/CN=127.0.0.1\" -passin pass:crew"
    )
  )
  system(
    paste(
      "openssl x509 -req -days 365 -in fd.csr -signkey",
      "fd.key -out fd.crt -passin pass:crew"
    )
  )
  expect_silent(
    tmp <- mirai::daemons(
      n = 1L,
      url = "wss://127.0.0.1:0",
      dispatcher = TRUE,
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
  mirai::daemons(n = 0L)
})

test_that("crew_tls() custom works on real credentials", {
  skip_on_os("windows")
  on.exit(unlink(c("fd.key", "fd.csr", "fd.crt")))
  system(
    paste(
      "openssl genpkey -out fd.key -algorithm RSA",
      "-outform PEM -pkeyopt rsa_keygen_bits:2048"
    )
  )
  system("openssl req -new -key fd.key -out fd.csr -subj \"/CN=127.0.0.1\"")
  system("openssl x509 -req -days 365 -in fd.csr -signkey fd.key -out fd.crt")
  tls <- crew_tls(mode = "custom", key = "fd.key", certificates = "fd.crt")
  expect_silent(tls$validate())
  x <- crew_controller_local(tls = tls)
  x$start()
  x$push("57")
  x$wait()
  expect_equal(x$pop()$result[[1L]], "57")
})

test_that("crew_tls() custom works on real credentials with custom password", {
  skip_on_os("windows")
  on.exit(unlink(c("fd.key", "fd.csr", "fd.crt")))
  system(
    paste(
      "openssl genpkey -out fd.key -algorithm RSA -outform PEM",
      "-pkeyopt rsa_keygen_bits:2048 -des3 -pass pass:crew"
    )
  )
  system(
    paste(
      "openssl req -new -key fd.key -out fd.csr",
      "-subj \"/CN=127.0.0.1\" -passin pass:crew"
    )
  )
  system(
    paste(
      "openssl x509 -req -days 365 -in fd.csr -signkey",
      "fd.key -out fd.crt -passin pass:crew"
    )
  )
  tls <- crew_tls(
    mode = "custom",
    key = "fd.key",
    password = "crew",
    certificates = "fd.crt"
  )
  expect_silent(tls$validate())
  x <- crew_controller_local(tls = tls)
  x$start()
  x$push("57")
  x$wait()
  expect_equal(x$pop()$result[[1L]], "57")
})

test_that("crew_tls() automatic", {
  x <- crew_controller_local(tls = crew_tls(mode = "automatic"))
  x$start()
  x$push("57")
  x$wait()
  expect_equal(x$pop()$result[[1L]], "57")
})
