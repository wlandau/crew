test_that("tls_config() custom works on real credentials", {
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
  expect_silent(
    crew_tls(
      mode = "custom",
      key = "fd.key",
      certificates = "fd.crt"
    )
  )
})
