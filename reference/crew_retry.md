# Retry code.

Repeatedly retry a function while it keeps returning `FALSE` and exit
the loop when it returns `TRUE`

## Usage

``` r
crew_retry(
  fun,
  args = list(),
  seconds_interval = 0.25,
  seconds_timeout = 60,
  max_tries = Inf,
  error = TRUE,
  message = character(0),
  envir = parent.frame(),
  assertions = TRUE
)
```

## Arguments

- fun:

  Function that returns `FALSE` to keep waiting or `TRUE` to stop
  waiting.

- args:

  A named list of arguments to `fun`.

- seconds_interval:

  Nonnegative numeric of length 1, number of seconds to wait between
  calls to `fun`.

- seconds_timeout:

  Nonnegative numeric of length 1, number of seconds to loop before
  timing out.

- max_tries:

  Maximum number of calls to `fun` to try before giving up.

- error:

  Whether to throw an error on a timeout or max tries.

- message:

  Character of length 1, optional error message if the wait times out.

- envir:

  Environment to evaluate `fun`.

- assertions:

  `TRUE` to run assertions to check if arguments are valid, `FALSE`
  otherwise. `TRUE` is recommended for users.

## Value

`NULL` (invisibly).

## See also

Other utility:
[`crew_assert()`](https://wlandau.github.io/crew/reference/crew_assert.md),
[`crew_clean()`](https://wlandau.github.io/crew/reference/crew_clean.md),
[`crew_deprecate()`](https://wlandau.github.io/crew/reference/crew_deprecate.md),
[`crew_eval()`](https://wlandau.github.io/crew/reference/crew_eval.md),
[`crew_random_name()`](https://wlandau.github.io/crew/reference/crew_random_name.md),
[`crew_terminate_process()`](https://wlandau.github.io/crew/reference/crew_terminate_process.md),
[`crew_terminate_signal()`](https://wlandau.github.io/crew/reference/crew_terminate_signal.md),
[`crew_worker()`](https://wlandau.github.io/crew/reference/crew_worker.md)

## Examples

``` r
crew_retry(fun = function() TRUE)
```
