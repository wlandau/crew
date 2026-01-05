# Options for logging resource usage metrics.

`crew_options_metrics()` configures the `crew` to record resource usage
metrics (such as CPU and memory usage) for each running worker. To be
activate resource usage logging, the `autometric` R package version
0.1.0 or higher must be installed.

Logging happens in the background (through a detached POSIX) so as not
to disrupt the R session. On Unix-like systems, `crew_options_metrics()`
can specify `/dev/stdout` or `/dev/stderr` as the log files, which will
redirect output to existing logs you are already using.
[`autometric::log_read()`](https://wlandau.github.io/autometric/reference/log_read.html)
and
[`autometric::log_plot()`](https://wlandau.github.io/autometric/reference/log_plot.html)
can read and visualize resource usage data from multiple log files, even
if those files are mixed with other messages.

## Usage

``` r
crew_options_metrics(path = NULL, seconds_interval = 5)
```

## Arguments

- path:

  Where to write resource metric log entries for workers. `path = NULL`
  disables logging. `path` equal to `"/dev/stdout"` (or `"/dev/stderr"`)
  sends log messages to the standard output (or standard error) streams,
  which is recommended on Unix-like systems because then output will go
  to the existing log files already configured for the controller, e.g.
  through
  [`crew_options_local()`](https://wlandau.github.io/crew/reference/crew_options_local.md)
  in the case of
  [`crew_controller_local()`](https://wlandau.github.io/crew/reference/crew_controller_local.md).
  If `path` is not `NULL`, `"/dev/stdout"`, or `"/dev/stderr"`, it
  should be a directory path, in which case each worker instance will
  write to a new file in that directory.

  After running enough tasks in `crew`, you can call
  `autometric::log_read(path)` to read all the data from all the log
  files in the files or directories at `path`, even if the logs files
  are mixed with other kinds of messages. Pass that data into
  [`autometric::log_plot()`](https://wlandau.github.io/autometric/reference/log_plot.html)
  to visualize it.

- seconds_interval:

  Positive number, seconds between resource metric log entries written
  to `path`.

## Value

A classed list of options for logging resource usage metrics.

## See also

Other options:
[`crew_options_local()`](https://wlandau.github.io/crew/reference/crew_options_local.md)

## Examples

``` r
  crew_options_metrics()
#> $path
#> NULL
#> 
#> $seconds_interval
#> [1] 5
#> 
#> attr(,"class")
#> [1] "crew_options_metrics" "crew_options"        
```
