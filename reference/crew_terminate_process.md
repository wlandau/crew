# Manually terminate a local process.

Manually terminate a local process.

## Usage

``` r
crew_terminate_process(pid)
```

## Arguments

- pid:

  Integer of length 1, process ID to terminate.

## Value

`NULL` (invisibly).

## See also

Other utility:
[`crew_assert()`](https://wlandau.github.io/crew/reference/crew_assert.md),
[`crew_clean()`](https://wlandau.github.io/crew/reference/crew_clean.md),
[`crew_deprecate()`](https://wlandau.github.io/crew/reference/crew_deprecate.md),
[`crew_eval()`](https://wlandau.github.io/crew/reference/crew_eval.md),
[`crew_random_name()`](https://wlandau.github.io/crew/reference/crew_random_name.md),
[`crew_retry()`](https://wlandau.github.io/crew/reference/crew_retry.md),
[`crew_terminate_signal()`](https://wlandau.github.io/crew/reference/crew_terminate_signal.md),
[`crew_worker()`](https://wlandau.github.io/crew/reference/crew_worker.md)

## Examples

``` r
if (identical(Sys.getenv("CREW_EXAMPLES"), "true")) {
process <- processx::process$new("sleep", "60")
process$is_alive()
crew_terminate_process(pid = process$get_pid())
process$is_alive()
}
```
