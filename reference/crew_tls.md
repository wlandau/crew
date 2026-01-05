# Configure TLS.

Create an `R6` object with transport layer security (TLS) configuration
for `crew`.

## Usage

``` r
crew_tls(
  mode = "none",
  key = NULL,
  password = NULL,
  certificates = NULL,
  validate = TRUE
)
```

## Arguments

- mode:

  Character of length 1. Must be one of the following:

  - `"none"`: disable TLS configuration.

  - `"automatic"`: let `mirai` create a one-time key pair with a
    self-signed certificate.

  - `"custom"`: manually supply a private key pair, an optional password
    for the private key, a certificate, an optional revocation list.

- key:

  If `mode` is `"none"` or `"automatic"`, then `key` is `NULL`. If
  `mode` is `"custom"`, then `key` is a character of length 1 with the
  file path to the private key file.

- password:

  If `mode` is `"none"` or `"automatic"`, then `password` is `NULL`. If
  `mode` is `"custom"` and the private key is not encrypted, then
  `password` is still `NULL`. If `mode` is `"custom"` and the private
  key is encrypted, then `password` is a character of length 1 the the
  password of the private key. In this case, DO NOT SAVE THE PASSWORD IN
  YOUR R CODE FILES. See the `keyring` R package for solutions.

- certificates:

  If `mode` is `"none"` or `"automatic"`, then `certificates` is `NULL`.
  If `mode` is `"custom"`, then `certificates` is a character vector of
  file paths to certificate files (signed public keys). If the
  certificate is self-signed or if it is directly signed by a
  certificate authority (CA), then only the certificate of the CA is
  needed. But if you have a whole certificate chain which begins at your
  own certificate and ends with the CA, then you can supply the whole
  certificate chain as a character vector of file paths, beginning at
  your own certificate and ending with the certificate of the CA.

- validate:

  Logical of length 1, whether to validate the configuration object on
  creation. If `FALSE`, then `validate()` can be called later on.

## Value

An `R6` object with TLS configuration settings and methods.

## Details

`crew_tls()` objects are input to the `tls` argument of
[`crew_client()`](https://wlandau.github.io/crew/reference/crew_client.md),
[`crew_controller_local()`](https://wlandau.github.io/crew/reference/crew_controller_local.md),
etc. See <https://wlandau.github.io/crew/articles/risks.html> for
details.

## See also

Other tls:
[`crew_class_tls`](https://wlandau.github.io/crew/reference/crew_class_tls.md)

## Examples

``` r
crew_tls(mode = "automatic")
#> <crew_class_tls>
#>   Public:
#>     certificates: active binding
#>     client: function () 
#>     initialize: function (mode = NULL, key = NULL, password = NULL, certificates = NULL) 
#>     key: active binding
#>     mode: active binding
#>     password: active binding
#>     private: environment
#>     self: crew_class_tls, R6
#>     url: function (host, port) 
#>     validate: function (test = TRUE) 
#>     worker: function (profile) 
#>   Private:
#>     .certificates: NULL
#>     .key: NULL
#>     .mode: automatic
#>     .password: NULL
#>     .read_certificates: function () 
#>     .read_files: function (files) 
#>     .read_key: function () 
#>     .validate_mode_automatic: function () 
#>     .validate_mode_custom: function () 
```
