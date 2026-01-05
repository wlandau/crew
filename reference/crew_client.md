# Create a client object.

Create an `R6` wrapper object to manage the `mirai` client.

## Usage

``` r
crew_client(
  name = NULL,
  workers = NULL,
  host = NULL,
  port = NULL,
  serialization = NULL,
  profile = crew::crew_random_name(),
  tls = crew::crew_tls(),
  tls_enable = NULL,
  tls_config = NULL,
  seconds_interval = 0.25,
  seconds_timeout = 60,
  retry_tasks = NULL
)
```

## Arguments

- name:

  Deprecated on 2025-01-14 (`crew` version 0.10.2.9002).

- workers:

  Deprecated on 2025-01-13 (`crew` version 0.10.2.9002).

- host:

  IP address of the `mirai` client to send and receive tasks. If `NULL`,
  the host defaults to `nanonext::ip_addr()[1]`.

- port:

  TCP port to listen for the workers. If `NULL`, then an available
  ephemeral port is automatically chosen. Controllers running
  simultaneously on the same computer (as in a controller group) must
  not share the same TCP port.

- serialization:

  Either `NULL` (default) or an object produced by
  [`mirai::serial_config()`](https://nanonext.r-lib.org/reference/serial_config.html)
  to control the serialization of data sent to workers. This can help
  with either more efficient data transfers or to preserve attributes of
  otherwise non-exportable objects (such as `torch` tensors or `arrow`
  tables). See
  [`?mirai::serial_config`](https://nanonext.r-lib.org/reference/serial_config.html)
  for details.

- profile:

  Character string, compute profile for
  [`mirai::daemons()`](https://mirai.r-lib.org/reference/daemons.html).

- tls:

  A TLS configuration object from
  [`crew_tls()`](https://wlandau.github.io/crew/reference/crew_tls.md).

- tls_enable:

  Deprecated on 2023-09-15 in version 0.4.1. Use argument `tls` instead.

- tls_config:

  Deprecated on 2023-09-15 in version 0.4.1. Use argument `tls` instead.

- seconds_interval:

  Number of seconds between polling intervals waiting for certain
  internal synchronous operations to complete, such as checking
  [`mirai::info()`](https://mirai.r-lib.org/reference/info.html)

- seconds_timeout:

  Number of seconds until timing out while waiting for certain
  synchronous operations to complete, such as checking
  [`mirai::info()`](https://mirai.r-lib.org/reference/info.html).

- retry_tasks:

  Deprecated on 2025-01-13 (`crew` version 0.10.2.9002).

## See also

Other client:
[`crew_class_client`](https://wlandau.github.io/crew/reference/crew_class_client.md)

## Examples

``` r
if (identical(Sys.getenv("CREW_EXAMPLES"), "true")) {
client <- crew_client()
client$start()
client$summary()
client$terminate()
}
```
