# Create an abstract launcher.

This function is useful for inheriting argument documentation in
functions that create custom third-party launchers. See
`@inheritParams crew::crew_launcher` in the source code file of
[`crew_launcher_local()`](https://wlandau.github.io/crew/reference/crew_launcher_local.md).

## Usage

``` r
crew_launcher(
  name = NULL,
  workers = 1L,
  seconds_interval = 0.25,
  seconds_timeout = 60,
  seconds_launch = 30,
  seconds_idle = 300,
  seconds_wall = Inf,
  seconds_exit = NULL,
  tasks_max = Inf,
  tasks_timers = 0L,
  reset_globals = NULL,
  reset_packages = NULL,
  reset_options = NULL,
  garbage_collection = NULL,
  crashes_error = NULL,
  launch_max = NULL,
  tls = crew::crew_tls(),
  processes = NULL,
  r_arguments = c("--no-save", "--no-restore"),
  options_metrics = crew::crew_options_metrics()
)
```

## Arguments

- name:

  Character string, name of the launcher. If the name is `NULL`, then a
  name is automatically generated when the launcher starts.

- workers:

  Maximum number of workers to run concurrently when auto-scaling,
  excluding task retries and manual calls to `launch()`. Special workers
  allocated for task retries do not count towards this limit, so the
  number of workers running at a given time may exceed this maximum. A
  smaller number of workers may run if the number of executing tasks is
  smaller than the supplied value of the `workers` argument.

- seconds_interval:

  Number of seconds between polling intervals waiting for certain
  internal synchronous operations to complete. In certain cases,
  exponential backoff is used with this argument passed to `seconds_max`
  in a
  [`crew_throttle()`](https://wlandau.github.io/crew/reference/crew_throttle.md)
  object.

- seconds_timeout:

  Number of seconds until timing out while waiting for certain
  synchronous operations to complete, such as checking
  [`mirai::info()`](https://mirai.r-lib.org/reference/info.html).

- seconds_launch:

  Seconds of startup time to allow. A worker is unconditionally assumed
  to be alive from the moment of its launch until `seconds_launch`
  seconds later. After `seconds_launch` seconds, the worker is only
  considered alive if it is actively connected to its assign websocket.

- seconds_idle:

  Maximum number of seconds that a worker can idle since the completion
  of the last task. If exceeded, the worker exits. But the timer does
  not launch until `tasks_timers` tasks have completed. See the
  `idletime` argument of
  [`mirai::daemon()`](https://mirai.r-lib.org/reference/daemon.html).
  `crew` does not excel with perfectly transient workers because it does
  not micromanage the assignment of tasks to workers, so please allow
  enough idle time for a new worker to be delegated a new task.

- seconds_wall:

  Soft wall time in seconds. The timer does not launch until
  `tasks_timers` tasks have completed. See the `walltime` argument of
  [`mirai::daemon()`](https://mirai.r-lib.org/reference/daemon.html).

- seconds_exit:

  Deprecated on 2023-09-21 in version 0.5.0.9002. No longer necessary.

- tasks_max:

  Maximum number of tasks that a worker will do before exiting. Also
  determines how often the controller auto-scales. See the Auto-scaling
  section for details.

- tasks_timers:

  Number of tasks to do before activating the timers for `seconds_idle`
  and `seconds_wall`. See the `timerstart` argument of
  [`mirai::daemon()`](https://mirai.r-lib.org/reference/daemon.html).

- reset_globals:

  Deprecated on 2025-05-30 (`crew` version 1.1.2.9004). Please use the
  `reset_globals` option of
  [`crew_controller()`](https://wlandau.github.io/crew/reference/crew_controller.md)
  instead.

- reset_packages:

  Deprecated on 2025-05-30 (`crew` version 1.1.2.9004). Please use the
  `reset_packages` option of
  [`crew_controller()`](https://wlandau.github.io/crew/reference/crew_controller.md)
  instead.

- reset_options:

  Deprecated on 2025-05-30 (`crew` version 1.1.2.9004). Please use the
  `reset_options` option of
  [`crew_controller()`](https://wlandau.github.io/crew/reference/crew_controller.md)
  instead.

- garbage_collection:

  Deprecated on 2025-05-30 (`crew` version 1.1.2.9004). Please use the
  `garbage_collection` option of
  [`crew_controller()`](https://wlandau.github.io/crew/reference/crew_controller.md)
  instead.

- crashes_error:

  Deprecated on 2025-01-13 (`crew` version 0.10.2.9002).

- launch_max:

  Deprecated on 2024-11-04 (`crew` version 0.10.2.9002).

- tls:

  A TLS configuration object from
  [`crew_tls()`](https://wlandau.github.io/crew/reference/crew_tls.md).

- processes:

  Deprecated on 2025-08-27 (`crew` version 1.2.1.9009).

- r_arguments:

  Optional character vector of command line arguments to pass to
  `Rscript` (non-Windows) or `Rscript.exe` (Windows) when starting a
  worker. Example:
  `r_arguments = c("--vanilla", "--max-connections=32")`.

- options_metrics:

  Either `NULL` to opt out of resource metric logging for workers, or an
  object from
  [`crew_options_metrics()`](https://wlandau.github.io/crew/reference/crew_options_metrics.md)
  to enable and configure resource metric logging for workers. For
  resource logging to run, the `autometric` R package version 0.1.0 or
  higher must be installed.

## Auto-scaling

`crew` launchers implement auto-scaling in the
[`scale()`](https://rdrr.io/r/base/scale.html) method. When the task
load increases, the number of workers increases in response to demand.
When the task load decreases, the workers start to exit. This behavior
happens dynamically over the course of a workflow, and it can be tuned
with arguments `seconds_interval`, `seconds_wall`, and `tasks_max`.

`tasks_max` is special: it determines not only the number of tasks a
worker runs before exiting, it also determines how often auto-scaling
runs. If `tasks_max` is finite, then `crew` uses an aggressive
deterministic exponential backoff algorithm to determine how frequently
to auto-scale (see
[`crew_throttle()`](https://wlandau.github.io/crew/reference/crew_throttle.md)).
But if `tasks_max` is `Inf`, then `crew` only scales at equally-spaced
time intervals of `seconds_interval` to allow enough pending tasks to
accumulate for job arrays. This last part is important because
auto-scaling too frequently could lead to hundreds of separate job
arrays with only job per array (as opposed to the desired outcome of 1
or 2 arrays with many jobs each).

## See also

Other launcher:
[`crew_class_launcher`](https://wlandau.github.io/crew/reference/crew_class_launcher.md)

## Examples

``` r
if (identical(Sys.getenv("CREW_EXAMPLES"), "true")) {
client <- crew_client()
client$start()
launcher <- crew_launcher_local()
launcher$start(url = client$url, profile = client$profile)
launcher$launch()
task <- mirai::mirai("result", .compute = client$profile)
mirai::call_mirai(task)
task$data
client$terminate()
}
```
