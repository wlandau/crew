# Crew worker.

Launches a `crew` worker which runs a `mirai` daemon. Not a user-side
function. Users should not call `crew_worker()` directly. See launcher
plugins like
[`crew_launcher_local()`](https://wlandau.github.io/crew/reference/crew_launcher_local.md)
for examples.

## Usage

``` r
crew_worker(
  settings,
  controller,
  options_metrics = crew::crew_options_metrics()
)
```

## Arguments

- settings:

  Named list of arguments to
  [`mirai::daemon()`](https://mirai.r-lib.org/reference/daemon.html).

- controller:

  Character string, name of the controller.

- options_metrics:

  Either `NULL` to opt out of resource metric logging for workers, or an
  object from
  [`crew_options_metrics()`](https://wlandau.github.io/crew/reference/crew_options_metrics.md)
  to enable and configure resource metric logging for workers. For
  resource logging to run, the `autometric` R package version 0.1.0 or
  higher must be installed.

## Value

`NULL` (invisibly)

## See also

Other utility:
[`crew_assert()`](https://wlandau.github.io/crew/reference/crew_assert.md),
[`crew_clean()`](https://wlandau.github.io/crew/reference/crew_clean.md),
[`crew_deprecate()`](https://wlandau.github.io/crew/reference/crew_deprecate.md),
[`crew_eval()`](https://wlandau.github.io/crew/reference/crew_eval.md),
[`crew_random_name()`](https://wlandau.github.io/crew/reference/crew_random_name.md),
[`crew_retry()`](https://wlandau.github.io/crew/reference/crew_retry.md),
[`crew_terminate_process()`](https://wlandau.github.io/crew/reference/crew_terminate_process.md),
[`crew_terminate_signal()`](https://wlandau.github.io/crew/reference/crew_terminate_signal.md)
