# Create a `crew` relay object.

Create an `R6` `crew` relay object.

## Usage

``` r
crew_relay(throttle = crew_throttle())
```

## Arguments

- throttle:

  A
  [`crew_throttle()`](https://wlandau.github.io/crew/reference/crew_throttle.md)
  object.

## Value

An `R6` `crew` relay object.

## Details

A `crew` relay object keeps the signaling relationships among condition
variables.

## See also

Other relay:
[`crew_class_relay`](https://wlandau.github.io/crew/reference/crew_class_relay.md)

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
