# Local `crew` launcher options.

Options for the local `crew` launcher.

## Usage

``` r
crew_options_local(log_directory = NULL, log_join = TRUE)
```

## Arguments

- log_directory:

  Either `NULL` or a character of length 1 with the file path to a
  directory to write worker-specific log files with standard output and
  standard error messages. Each log file represents a single *instance*
  of a running worker, so there will be more log files if a given worker
  starts and terminates a lot. Set to `NULL` to suppress log files
  (default).

- log_join:

  Logical of length 1. If `TRUE`, `crew` will write standard output and
  standard error to the same log file for each worker instance. If
  `FALSE`, then they these two streams will go to different log files
  with informative suffixes.

## Value

A classed list of options for the local launcher.

## See also

Other options:
[`crew_options_metrics()`](https://wlandau.github.io/crew/reference/crew_options_metrics.md)

## Examples

``` r
  crew_options_local()
#> $log_directory
#> NULL
#> 
#> $log_join
#> [1] TRUE
#> 
#> attr(,"class")
#> [1] "crew_options_local" "crew_options"      
```
