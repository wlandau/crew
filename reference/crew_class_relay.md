# `R6` relay class.

`R6` class for relay configuration.

## Details

See
[`crew_relay()`](https://wlandau.github.io/crew/reference/crew_relay.md).

## See also

Other relay:
[`crew_relay()`](https://wlandau.github.io/crew/reference/crew_relay.md)

## Active bindings

- `condition`:

  Main condition variable.

- `from`:

  Condition variable to relay from.

- `to`:

  Condition variable to relay to.

- `throttle`:

  A
  [`crew_throttle()`](https://wlandau.github.io/crew/reference/crew_throttle.md)
  object for `wait()`.

## Methods

### Public methods

- [`crew_class_relay$new()`](#method-crew_class_relay-new)

- [`crew_class_relay$validate()`](#method-crew_class_relay-validate)

- [`crew_class_relay$start()`](#method-crew_class_relay-start)

- [`crew_class_relay$terminate()`](#method-crew_class_relay-terminate)

- [`crew_class_relay$set_from()`](#method-crew_class_relay-set_from)

- [`crew_class_relay$set_to()`](#method-crew_class_relay-set_to)

- [`crew_class_relay$wait()`](#method-crew_class_relay-wait)

------------------------------------------------------------------------

### Method `new()`

Relay constructor.

#### Usage

    crew_class_relay$new(throttle)

#### Arguments

- `throttle`:

  A
  [`crew_throttle()`](https://wlandau.github.io/crew/reference/crew_throttle.md)
  object.

#### Returns

A
[`crew_relay()`](https://wlandau.github.io/crew/reference/crew_relay.md)
object.

------------------------------------------------------------------------

### Method `validate()`

Validate the object.

#### Usage

    crew_class_relay$validate()

#### Returns

`NULL` (invisibly).

------------------------------------------------------------------------

### Method [`start()`](https://rdrr.io/r/stats/start.html)

Start the relay object.

#### Usage

    crew_class_relay$start()

#### Returns

`NULL` (invisibly).

------------------------------------------------------------------------

### Method `terminate()`

Terminate the relay object.

#### Usage

    crew_class_relay$terminate()

#### Returns

`NULL` (invisibly).

------------------------------------------------------------------------

### Method `set_from()`

Set the condition variable to relay from.

#### Usage

    crew_class_relay$set_from(from)

#### Arguments

- `from`:

  Condition variable to relay from.

#### Returns

`NULL` (invisibly).

------------------------------------------------------------------------

### Method `set_to()`

Set the condition variable to relay to.

#### Usage

    crew_class_relay$set_to(to)

#### Arguments

- `to`:

  Condition variable to relay to.

#### Returns

`NULL` (invisibly).

------------------------------------------------------------------------

### Method `wait()`

Wait until an unobserved task resolves or the timeout is reached. Use
the throttle to determine the waiting time.

#### Usage

    crew_class_relay$wait()

#### Returns

`NULL` (invisibly).

## Examples

``` r
crew_relay()
#> <crew_class_relay>
#>   Public:
#>     condition: active binding
#>     from: active binding
#>     initialize: function (throttle) 
#>     private: environment
#>     self: crew_class_relay, R6
#>     set_from: function (from) 
#>     set_to: function (to) 
#>     start: function () 
#>     terminate: function () 
#>     throttle: active binding
#>     to: active binding
#>     validate: function () 
#>     wait: function () 
#>   Private:
#>     .condition: NULL
#>     .from: NULL
#>     .throttle: crew_class_throttle, R6
#>     .to: NULL
```
