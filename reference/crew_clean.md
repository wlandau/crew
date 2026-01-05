# Deprecated: terminate dispatchers and/or workers

Deprecated on 2025-08-26 in `crew` version 1.2.1.9006. Please use
[`crew_monitor_local()`](https://wlandau.github.io/crew/reference/crew_monitor_local.md)
instead.

## Usage

``` r
crew_clean(
  dispatchers = TRUE,
  workers = TRUE,
  user = ps::ps_username(),
  seconds_interval = 0.25,
  seconds_timeout = 60,
  verbose = TRUE
)
```

## Arguments

- dispatchers:

  Logical of length 1, whether to terminate dispatchers.

- workers:

  Logical of length 1, whether to terminate workers.

- user:

  Character of length 1. Terminate dispatchers and/or workers associated
  with this user name.

- seconds_interval:

  Seconds to wait between polling intervals waiting for a process to
  exit.

- seconds_timeout:

  Seconds to wait for a process to exit.

- verbose:

  Logical of length 1, whether to print an informative message every
  time a process is terminated.

## Value

`NULL` (invisibly). If `verbose` is `TRUE`, it does print out a message
for every terminated process.

## Details

Behind the scenes, `mirai` uses an external R process called a
"dispatcher" to send tasks to `crew` workers. This dispatcher usually
shuts down when you terminate the controller or quit your R session, but
sometimes it lingers. Likewise, sometimes `crew` workers do not shut
down on their own. The `crew_clean()` function searches the process
table on your local machine and manually terminates any `mirai`
dispatchers and `crew` workers associated with your user name (or the
user name you select in the `user` argument. Unfortunately, it cannot
reach remote workers such as those launched by a `crew.cluster`
controller.

## See also

Other utility:
[`crew_assert()`](https://wlandau.github.io/crew/reference/crew_assert.md),
[`crew_deprecate()`](https://wlandau.github.io/crew/reference/crew_deprecate.md),
[`crew_eval()`](https://wlandau.github.io/crew/reference/crew_eval.md),
[`crew_random_name()`](https://wlandau.github.io/crew/reference/crew_random_name.md),
[`crew_retry()`](https://wlandau.github.io/crew/reference/crew_retry.md),
[`crew_terminate_process()`](https://wlandau.github.io/crew/reference/crew_terminate_process.md),
[`crew_terminate_signal()`](https://wlandau.github.io/crew/reference/crew_terminate_signal.md),
[`crew_worker()`](https://wlandau.github.io/crew/reference/crew_worker.md)

## Examples

``` r
if (identical(Sys.getenv("CREW_EXAMPLES"), "true")) {
crew_clean()
}
```
