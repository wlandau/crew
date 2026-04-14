# Create a stateful throttling object.

Create an `R6` object for throttling.

## Usage

``` r
crew_throttle(
  seconds_max = 1,
  seconds_min = 1e-06,
  seconds_start = seconds_min,
  base = 2
)
```

## Arguments

- seconds_max:

  Positive numeric scalar, maximum throttling interval

- seconds_min:

  Positive numeric scalar, minimum throttling interval.

- seconds_start:

  Positive numeric scalar, the initial wait time interval in seconds.
  The default is `min` because there is almost always auto-scaling to be
  done when the controller is created. `reset()` always sets the current
  wait interval back to `seconds_start`.

- base:

  Numeric scalar greater than 1, base of the exponential backoff
  algorithm. `increment()` multiplies the waiting interval by `base` and
  `decrement()` divides the waiting interval by `base`. The default
  `base` is 2, which specifies a binary exponential backoff algorithm.

## Value

An `R6` object with throttle configuration settings and methods.

## Details

Throttling is a technique that limits how often a function is called in
a given period of time. `crew_throttle()` objects support the `throttle`
argument of controller methods, which ensures auto-scaling does not
induce superfluous overhead. The throttle uses deterministic exponential
backoff algorithm (<https://en.wikipedia.org/wiki/Exponential_backoff>)
which increases wait times when there is nothing to do and decreases
wait times when there is something to do. The controller decreases or
increases the wait time with methods `accelerate()` and `decelerate()`
in the throttle object, respectively, by dividing or multiplying by
`base` (but keeping the wait time between `seconds_min` and
`seconds_max`). In practice, `crew` calls `reset()` instead of
[`update()`](https://rdrr.io/r/stats/update.html) in order to respond
quicker to surges of activity (see the
[`update()`](https://rdrr.io/r/stats/update.html) method).

## See also

Other throttle:
[`crew_class_throttle`](https://wlandau.github.io/crew/reference/crew_class_throttle.md)

## Examples

``` r
throttle <- crew_throttle(seconds_max = 1)
throttle$poll()
#> [1] FALSE
throttle$poll()
#> [1] TRUE
```
