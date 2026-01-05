# Random name

Generate a random string that can be used as a name for a worker or
task.

## Usage

``` r
crew_random_name(n = 8L)
```

## Arguments

- n:

  Number of bytes of information in the random string hashed to generate
  the name. Larger `n` is more likely to generate unique names, but it
  may be slower to compute.

## Value

A random character string.

## Details

The randomness is not reproducible and cannot be set with e.g.
[`set.seed()`](https://rdrr.io/r/base/Random.html) in R.

## See also

Other utility:
[`crew_assert()`](https://wlandau.github.io/crew/reference/crew_assert.md),
[`crew_clean()`](https://wlandau.github.io/crew/reference/crew_clean.md),
[`crew_deprecate()`](https://wlandau.github.io/crew/reference/crew_deprecate.md),
[`crew_eval()`](https://wlandau.github.io/crew/reference/crew_eval.md),
[`crew_retry()`](https://wlandau.github.io/crew/reference/crew_retry.md),
[`crew_terminate_process()`](https://wlandau.github.io/crew/reference/crew_terminate_process.md),
[`crew_terminate_signal()`](https://wlandau.github.io/crew/reference/crew_terminate_signal.md),
[`crew_worker()`](https://wlandau.github.io/crew/reference/crew_worker.md)

## Examples

``` r
crew_random_name()
#> [1] "25caf2562bc14c99"
```
