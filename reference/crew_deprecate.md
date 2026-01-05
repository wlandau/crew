# Deprecate a `crew` feature.

Show an informative warning when a `crew` feature is deprecated.

## Usage

``` r
crew_deprecate(
  name,
  date,
  version,
  alternative,
  condition = "warning",
  value = "x",
  skip_cran = FALSE,
  frequency = "always"
)
```

## Arguments

- name:

  Name of the feature (function or argument) to deprecate.

- date:

  Date of deprecation.

- version:

  Package version when deprecation was instated.

- alternative:

  Message about an alternative.

- condition:

  Either "warning" or "message" to indicate the type of condition thrown
  on deprecation.

- value:

  Value of the object. Deprecation is skipped if `value` is `NULL`.

- skip_cran:

  Logical of length 1, whether to skip the deprecation warning or
  message on CRAN.

- frequency:

  Character of length 1, passed to the `.frequency` argument of
  [`rlang::warn()`](https://rlang.r-lib.org/reference/abort.html).

## Value

`NULL` (invisibly). Throws a warning if a feature is deprecated.

## See also

Other utility:
[`crew_assert()`](https://wlandau.github.io/crew/reference/crew_assert.md),
[`crew_clean()`](https://wlandau.github.io/crew/reference/crew_clean.md),
[`crew_eval()`](https://wlandau.github.io/crew/reference/crew_eval.md),
[`crew_random_name()`](https://wlandau.github.io/crew/reference/crew_random_name.md),
[`crew_retry()`](https://wlandau.github.io/crew/reference/crew_retry.md),
[`crew_terminate_process()`](https://wlandau.github.io/crew/reference/crew_terminate_process.md),
[`crew_terminate_signal()`](https://wlandau.github.io/crew/reference/crew_terminate_signal.md),
[`crew_worker()`](https://wlandau.github.io/crew/reference/crew_worker.md)

## Examples

``` r
suppressWarnings(
  crew_deprecate(
    name = "auto_scale",
    date = "2023-05-18",
    version = "0.2.0",
    alternative = "use the scale argument of push(), pop(), and wait()."
  )
)
```
