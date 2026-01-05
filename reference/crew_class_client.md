# `R6` client class.

`R6` class for `mirai` clients.

## Details

See
[`crew_client()`](https://wlandau.github.io/crew/reference/crew_client.md).

## See also

Other client:
[`crew_client()`](https://wlandau.github.io/crew/reference/crew_client.md)

## Active bindings

- `host`:

  See
  [`crew_client()`](https://wlandau.github.io/crew/reference/crew_client.md).

- `port`:

  See
  [`crew_client()`](https://wlandau.github.io/crew/reference/crew_client.md).

- `tls`:

  See
  [`crew_client()`](https://wlandau.github.io/crew/reference/crew_client.md).

- `serialization`:

  See
  [`crew_client()`](https://wlandau.github.io/crew/reference/crew_client.md).

- `profile`:

  Compute profile of the client.

- `seconds_interval`:

  See
  [`crew_client()`](https://wlandau.github.io/crew/reference/crew_client.md).

- `seconds_timeout`:

  See
  [`crew_client()`](https://wlandau.github.io/crew/reference/crew_client.md).

- `relay`:

  Relay object for event-driven programming on a downstream condition
  variable.

- `started`:

  Whether the client is started.

- `url`:

  Client websocket URL.

## Methods

### Public methods

- [`crew_class_client$new()`](#method-crew_class_client-new)

- [`crew_class_client$validate()`](#method-crew_class_client-validate)

- [`crew_class_client$set_started()`](#method-crew_class_client-set_started)

- [`crew_class_client$start()`](#method-crew_class_client-start)

- [`crew_class_client$terminate()`](#method-crew_class_client-terminate)

- [`crew_class_client$status()`](#method-crew_class_client-status)

- [`crew_class_client$pids()`](#method-crew_class_client-pids)

------------------------------------------------------------------------

### Method `new()`

`mirai` client constructor.

#### Usage

    crew_class_client$new(
      host = NULL,
      port = NULL,
      tls = NULL,
      serialization = NULL,
      profile = NULL,
      seconds_interval = NULL,
      seconds_timeout = NULL,
      relay = NULL
    )

#### Arguments

- `host`:

  Argument passed from
  [`crew_client()`](https://wlandau.github.io/crew/reference/crew_client.md).

- `port`:

  Argument passed from
  [`crew_client()`](https://wlandau.github.io/crew/reference/crew_client.md).

- `tls`:

  Argument passed from
  [`crew_client()`](https://wlandau.github.io/crew/reference/crew_client.md).

- `serialization`:

  Argument passed from
  [`crew_client()`](https://wlandau.github.io/crew/reference/crew_client.md).

- `profile`:

  Argument passed from
  [`crew_client()`](https://wlandau.github.io/crew/reference/crew_client.md).

- `seconds_interval`:

  Argument passed from
  [`crew_client()`](https://wlandau.github.io/crew/reference/crew_client.md).

- `seconds_timeout`:

  Argument passed from
  [`crew_client()`](https://wlandau.github.io/crew/reference/crew_client.md).

- `relay`:

  Argument passed from
  [`crew_client()`](https://wlandau.github.io/crew/reference/crew_client.md).

#### Returns

An `R6` object with the client.

#### Examples

    if (identical(Sys.getenv("CREW_EXAMPLES"), "true")) {
    client <- crew_client()
    client$start()
    client$log()
    client$terminate()
    }

------------------------------------------------------------------------

### Method `validate()`

Validate the client.

#### Usage

    crew_class_client$validate()

#### Returns

`NULL` (invisibly).

------------------------------------------------------------------------

### Method `set_started()`

Register the client as started.

#### Usage

    crew_class_client$set_started()

#### Details

Exported to implement the sequential controller. Only meant to be called
manually inside the client or the sequential controller.

#### Returns

`NULL` (invisibly).

------------------------------------------------------------------------

### Method [`start()`](https://rdrr.io/r/stats/start.html)

Start listening for workers on the available sockets.

#### Usage

    crew_class_client$start()

#### Returns

`NULL` (invisibly).

------------------------------------------------------------------------

### Method `terminate()`

Stop the mirai client and disconnect from the worker websockets.

#### Usage

    crew_class_client$terminate()

#### Returns

`NULL` (invisibly).

------------------------------------------------------------------------

### Method `status()`

Get the counters from
[`mirai::info()`](https://mirai.r-lib.org/reference/info.html).

#### Usage

    crew_class_client$status()

#### Returns

A named integer vector of task counts (awaiting, executing, completed)
as well as the number of worker connections.

------------------------------------------------------------------------

### Method `pids()`

Deprecated on 2025-08-26 in `crew` version 1.2.1.9005.

#### Usage

    crew_class_client$pids()

#### Returns

The integer process ID of the current process.

## Examples

``` r
if (identical(Sys.getenv("CREW_EXAMPLES"), "true")) {
client <- crew_client()
client$start()
client$log()
client$terminate()
}

## ------------------------------------------------
## Method `crew_class_client$new`
## ------------------------------------------------

if (identical(Sys.getenv("CREW_EXAMPLES"), "true")) {
client <- crew_client()
client$start()
client$log()
client$terminate()
}
```
