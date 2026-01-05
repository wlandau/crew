# Crew assertion

Assert that a condition is true.

## Usage

``` r
crew_assert(value = NULL, ..., message = NULL, envir = parent.frame())
```

## Arguments

- value:

  An object or condition.

- ...:

  Conditions that use the `"."` symbol to refer to the object.

- message:

  Optional message to print on error.

- envir:

  Environment to evaluate the condition.

## Value

`NULL` (invisibly). Throws an error if the condition is not true.

## See also

Other utility:
[`crew_clean()`](https://wlandau.github.io/crew/reference/crew_clean.md),
[`crew_deprecate()`](https://wlandau.github.io/crew/reference/crew_deprecate.md),
[`crew_eval()`](https://wlandau.github.io/crew/reference/crew_eval.md),
[`crew_random_name()`](https://wlandau.github.io/crew/reference/crew_random_name.md),
[`crew_retry()`](https://wlandau.github.io/crew/reference/crew_retry.md),
[`crew_terminate_process()`](https://wlandau.github.io/crew/reference/crew_terminate_process.md),
[`crew_terminate_signal()`](https://wlandau.github.io/crew/reference/crew_terminate_signal.md),
[`crew_worker()`](https://wlandau.github.io/crew/reference/crew_worker.md)

## Examples

``` r
crew_assert(1 < 2)
crew_assert("object", !anyNA(.), nzchar(.))
tryCatch(
  crew_assert(2 < 1),
  crew_error = function(condition) message("false")
)
#> false
```
