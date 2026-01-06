# `R6` throttle class.

`R6` class for throttle configuration.

## Details

See
[`crew_throttle()`](https://wlandau.github.io/crew/reference/crew_throttle.md).

## See also

Other throttle:
[`crew_throttle()`](https://wlandau.github.io/crew/reference/crew_throttle.md)

## Active bindings

- `seconds_max`:

  See
  [`crew_throttle()`](https://wlandau.github.io/crew/reference/crew_throttle.md).

- `seconds_min`:

  See
  [`crew_throttle()`](https://wlandau.github.io/crew/reference/crew_throttle.md).

- `seconds_start`:

  See
  [`crew_throttle()`](https://wlandau.github.io/crew/reference/crew_throttle.md).

- `base`:

  See
  [`crew_throttle()`](https://wlandau.github.io/crew/reference/crew_throttle.md).

- `seconds_interval`:

  Current wait time interval.

- `polled`:

  Positive numeric of length 1, millisecond timestamp of the last time
  `poll()` returned `TRUE`. `NULL` if `poll()` was never called on the
  current object.

## Methods

### Public methods

- [`crew_class_throttle$new()`](#method-crew_class_throttle-new)

- [`crew_class_throttle$validate()`](#method-crew_class_throttle-validate)

- [`crew_class_throttle$poll()`](#method-crew_class_throttle-poll)

- [`crew_class_throttle$accelerate()`](#method-crew_class_throttle-accelerate)

- [`crew_class_throttle$decelerate()`](#method-crew_class_throttle-decelerate)

- [`crew_class_throttle$reset()`](#method-crew_class_throttle-reset)

- [`crew_class_throttle$update()`](#method-crew_class_throttle-update)

------------------------------------------------------------------------

### Method `new()`

Throttle constructor.

#### Usage

    crew_class_throttle$new(
      seconds_max = NULL,
      seconds_min = NULL,
      seconds_start = NULL,
      base = NULL
    )

#### Arguments

- `seconds_max`:

  See
  [`crew_throttle()`](https://wlandau.github.io/crew/reference/crew_throttle.md).

- `seconds_min`:

  See
  [`crew_throttle()`](https://wlandau.github.io/crew/reference/crew_throttle.md).

- `seconds_start`:

  See
  [`crew_throttle()`](https://wlandau.github.io/crew/reference/crew_throttle.md).

- `base`:

  See
  [`crew_throttle()`](https://wlandau.github.io/crew/reference/crew_throttle.md).

#### Returns

An `R6` object with throttle configuration.

#### Examples

    throttle <- crew_throttle(seconds_max = 1)
    throttle$poll()
    throttle$poll()

------------------------------------------------------------------------

### Method `validate()`

Validate the object.

#### Usage

    crew_class_throttle$validate()

#### Returns

`NULL` (invisibly).

------------------------------------------------------------------------

### Method `poll()`

Poll the throttler.

#### Usage

    crew_class_throttle$poll()

#### Returns

`TRUE` if `poll()` did not return `TRUE` in the last `max` seconds,
`FALSE` otherwise.

------------------------------------------------------------------------

### Method `accelerate()`

Divide `seconds_interval` by `base`.

#### Usage

    crew_class_throttle$accelerate()

#### Returns

`NULL` (invisibly). Called for its side effects.

------------------------------------------------------------------------

### Method `decelerate()`

Multiply `seconds_interval` by `base`.

#### Usage

    crew_class_throttle$decelerate()

#### Returns

`NULL` (invisibly). Called for its side effects.

------------------------------------------------------------------------

### Method `reset()`

Reset the throttle object so the next `poll()` returns `TRUE`, and reset
the wait time interval to its initial value.

#### Usage

    crew_class_throttle$reset()

#### Returns

`NULL` (invisibly).

------------------------------------------------------------------------

### Method [`update()`](https://rdrr.io/r/stats/update.html)

Reset the throttle when there is activity and decelerate it gradually
when there is no activity.

#### Usage

    crew_class_throttle$update(activity)

#### Arguments

- `activity`:

  `TRUE` if there is activity, `FALSE` otherwise.

#### Returns

`NULL` (invisibly).

## Examples

``` r
throttle <- crew_throttle(seconds_max = 1)
throttle$poll()
#> [1] FALSE
throttle$poll()
#> [1] FALSE

## ------------------------------------------------
## Method `crew_class_throttle$new`
## ------------------------------------------------

throttle <- crew_throttle(seconds_max = 1)
throttle$poll()
#> [1] FALSE
throttle$poll()
#> [1] FALSE
```
