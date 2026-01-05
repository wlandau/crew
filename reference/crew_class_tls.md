# `R6` TLS class.

`R6` class for TLS configuration.

## Details

See
[`crew_tls()`](https://wlandau.github.io/crew/reference/crew_tls.md).

## See also

Other tls:
[`crew_tls()`](https://wlandau.github.io/crew/reference/crew_tls.md)

## Active bindings

- `mode`:

  See
  [`crew_tls()`](https://wlandau.github.io/crew/reference/crew_tls.md).

- `key`:

  See
  [`crew_tls()`](https://wlandau.github.io/crew/reference/crew_tls.md).

- `password`:

  See
  [`crew_tls()`](https://wlandau.github.io/crew/reference/crew_tls.md).

- `certificates`:

  See
  [`crew_tls()`](https://wlandau.github.io/crew/reference/crew_tls.md).

## Methods

### Public methods

- [`crew_class_tls$new()`](#method-crew_class_tls-new)

- [`crew_class_tls$validate()`](#method-crew_class_tls-validate)

- [`crew_class_tls$client()`](#method-crew_class_tls-client)

- [`crew_class_tls$worker()`](#method-crew_class_tls-worker)

- [`crew_class_tls$url()`](#method-crew_class_tls-url)

------------------------------------------------------------------------

### Method `new()`

TLS configuration constructor.

#### Usage

    crew_class_tls$new(
      mode = NULL,
      key = NULL,
      password = NULL,
      certificates = NULL
    )

#### Arguments

- `mode`:

  Argument passed from
  [`crew_tls()`](https://wlandau.github.io/crew/reference/crew_tls.md).

- `key`:

  Argument passed from
  [`crew_tls()`](https://wlandau.github.io/crew/reference/crew_tls.md).

- `password`:

  Argument passed from
  [`crew_tls()`](https://wlandau.github.io/crew/reference/crew_tls.md).

- `certificates`:

  Argument passed from
  [`crew_tls()`](https://wlandau.github.io/crew/reference/crew_tls.md).

#### Returns

An `R6` object with TLS configuration.

#### Examples

    crew_tls(mode = "automatic")

------------------------------------------------------------------------

### Method `validate()`

Validate the object.

#### Usage

    crew_class_tls$validate(test = TRUE)

#### Arguments

- `test`:

  Logical of length 1, whether to test the TLS configuration with
  [`nanonext::tls_config()`](https://nanonext.r-lib.org/reference/tls_config.html).

#### Returns

`NULL` (invisibly).

------------------------------------------------------------------------

### Method `client()`

TLS credentials for the `crew` client.

#### Usage

    crew_class_tls$client()

#### Returns

`NULL` or character vector, depending on the mode.

------------------------------------------------------------------------

### Method `worker()`

TLS credentials for `crew` workers.

#### Usage

    crew_class_tls$worker(profile)

#### Arguments

- `profile`:

  Character of length 1 with the `mirai` compute profile.

#### Returns

`NULL` or character vector, depending on the mode.

------------------------------------------------------------------------

### Method [`url()`](https://rdrr.io/r/base/connections.html)

Form the URL for `crew` worker connections.

#### Usage

    crew_class_tls$url(host, port)

#### Arguments

- `host`:

  Character string with the host name or IP address.

- `port`:

  Non-negative integer with the port number (0 to let NNG select a
  random ephemeral port).

#### Returns

Character string with the URL.

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

## ------------------------------------------------
## Method `crew_class_tls$new`
## ------------------------------------------------

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
