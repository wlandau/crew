# Get the termination signal.

Get a supported operating system signal for terminating a local process.

## Usage

``` r
crew_terminate_signal()
```

## Value

An integer of length 1:
[`tools::SIGTERM`](https://rdrr.io/r/tools/pskill.html) if your platform
supports `SIGTERM`. If not, then `crew_crew_terminate_signal()()` checks
`SIGQUIT`, then `SIGINT`, then `SIGKILL`, and then returns the first
signal it finds that your operating system can use.

## See also

Other utility:
[`crew_assert()`](https://wlandau.github.io/crew/reference/crew_assert.md),
[`crew_clean()`](https://wlandau.github.io/crew/reference/crew_clean.md),
[`crew_deprecate()`](https://wlandau.github.io/crew/reference/crew_deprecate.md),
[`crew_eval()`](https://wlandau.github.io/crew/reference/crew_eval.md),
[`crew_random_name()`](https://wlandau.github.io/crew/reference/crew_random_name.md),
[`crew_retry()`](https://wlandau.github.io/crew/reference/crew_retry.md),
[`crew_terminate_process()`](https://wlandau.github.io/crew/reference/crew_terminate_process.md),
[`crew_worker()`](https://wlandau.github.io/crew/reference/crew_worker.md)

## Examples

``` r
crew_terminate_signal()
#> [1] 15
```
