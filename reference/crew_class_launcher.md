# Launcher abstract class

`R6` abstract class to build other subclasses which launch and manage
workers.

## See also

Other launcher:
[`crew_launcher()`](https://wlandau.github.io/crew/reference/crew_launcher.md)

## Public fields

- `async`:

  Deprecated on 2025-08-27 (`crew` version 1.2.1.9009).

## Active bindings

- `name`:

  See
  [`crew_launcher()`](https://wlandau.github.io/crew/reference/crew_launcher.md).

- `workers`:

  See
  [`crew_launcher()`](https://wlandau.github.io/crew/reference/crew_launcher.md).

- `seconds_interval`:

  See
  [`crew_launcher()`](https://wlandau.github.io/crew/reference/crew_launcher.md).

- `seconds_timeout`:

  See
  [`crew_launcher()`](https://wlandau.github.io/crew/reference/crew_launcher.md).

- `seconds_launch`:

  See
  [`crew_launcher()`](https://wlandau.github.io/crew/reference/crew_launcher.md).

- `seconds_idle`:

  See
  [`crew_launcher()`](https://wlandau.github.io/crew/reference/crew_launcher.md).

- `seconds_wall`:

  See
  [`crew_launcher()`](https://wlandau.github.io/crew/reference/crew_launcher.md).

- `tasks_max`:

  See
  [`crew_launcher()`](https://wlandau.github.io/crew/reference/crew_launcher.md).

- `tasks_timers`:

  See
  [`crew_launcher()`](https://wlandau.github.io/crew/reference/crew_launcher.md).

- `tls`:

  See
  [`crew_launcher()`](https://wlandau.github.io/crew/reference/crew_launcher.md).

- `r_arguments`:

  See
  [`crew_launcher()`](https://wlandau.github.io/crew/reference/crew_launcher.md).

- `options_metrics`:

  See
  [`crew_launcher()`](https://wlandau.github.io/crew/reference/crew_launcher.md).

- `url`:

  Websocket URL for worker connections.

- `profile`:

  `mirai` compute profile of the launcher.

- `launches`:

  Data frame tracking worker launches with one row per launch. Each
  launch may create more than one worker. Old superfluous rows are
  periodically discarded for efficiency.

- `throttle`:

  A
  [`crew_throttle()`](https://wlandau.github.io/crew/reference/crew_throttle.md)
  object to throttle scaling.

- `failed`:

  Number of failed worker launches (launches that exceed
  `seconds_launch` seconds to dial in).

## Methods

### Public methods

- [`crew_class_launcher$new()`](#method-crew_class_launcher-new)

- [`crew_class_launcher$validate()`](#method-crew_class_launcher-validate)

- [`crew_class_launcher$poll()`](#method-crew_class_launcher-poll)

- [`crew_class_launcher$settings()`](#method-crew_class_launcher-settings)

- [`crew_class_launcher$call()`](#method-crew_class_launcher-call)

- [`crew_class_launcher$start()`](#method-crew_class_launcher-start)

- [`crew_class_launcher$terminate()`](#method-crew_class_launcher-terminate)

- [`crew_class_launcher$launch()`](#method-crew_class_launcher-launch)

- [`crew_class_launcher$launch_worker()`](#method-crew_class_launcher-launch_worker)

- [`crew_class_launcher$launch_workers()`](#method-crew_class_launcher-launch_workers)

- [`crew_class_launcher$scale()`](#method-crew_class_launcher-scale)

- [`crew_class_launcher$terminate_workers()`](#method-crew_class_launcher-terminate_workers)

- [`crew_class_launcher$crashes()`](#method-crew_class_launcher-crashes)

- [`crew_class_launcher$set_name()`](#method-crew_class_launcher-set_name)

- [`crew_class_launcher$clone()`](#method-crew_class_launcher-clone)

------------------------------------------------------------------------

### Method `new()`

Launcher constructor.

#### Usage

    crew_class_launcher$new(
      name = NULL,
      workers = NULL,
      seconds_interval = NULL,
      seconds_timeout = NULL,
      seconds_launch = NULL,
      seconds_idle = NULL,
      seconds_wall = NULL,
      seconds_exit = NULL,
      tasks_max = NULL,
      tasks_timers = NULL,
      reset_globals = NULL,
      reset_packages = NULL,
      reset_options = NULL,
      garbage_collection = NULL,
      crashes_error = NULL,
      launch_max = NULL,
      tls = NULL,
      processes = NULL,
      r_arguments = NULL,
      options_metrics = NULL
    )

#### Arguments

- `name`:

  See
  [`crew_launcher()`](https://wlandau.github.io/crew/reference/crew_launcher.md).

- `workers`:

  See
  [`crew_launcher()`](https://wlandau.github.io/crew/reference/crew_launcher.md).

- `seconds_interval`:

  See
  [`crew_launcher()`](https://wlandau.github.io/crew/reference/crew_launcher.md).

- `seconds_timeout`:

  See
  [`crew_launcher()`](https://wlandau.github.io/crew/reference/crew_launcher.md).

- `seconds_launch`:

  See
  [`crew_launcher()`](https://wlandau.github.io/crew/reference/crew_launcher.md).

- `seconds_idle`:

  See
  [`crew_launcher()`](https://wlandau.github.io/crew/reference/crew_launcher.md).

- `seconds_wall`:

  See
  [`crew_launcher()`](https://wlandau.github.io/crew/reference/crew_launcher.md).

- `seconds_exit`:

  See
  [`crew_launcher()`](https://wlandau.github.io/crew/reference/crew_launcher.md).

- `tasks_max`:

  See
  [`crew_launcher()`](https://wlandau.github.io/crew/reference/crew_launcher.md).

- `tasks_timers`:

  See
  [`crew_launcher()`](https://wlandau.github.io/crew/reference/crew_launcher.md).

- `reset_globals`:

  Deprecated. See
  [`crew_launcher()`](https://wlandau.github.io/crew/reference/crew_launcher.md).

- `reset_packages`:

  Deprecated. See
  [`crew_launcher()`](https://wlandau.github.io/crew/reference/crew_launcher.md).

- `reset_options`:

  Deprecated. See
  [`crew_launcher()`](https://wlandau.github.io/crew/reference/crew_launcher.md).

- `garbage_collection`:

  Deprecated. See
  [`crew_launcher()`](https://wlandau.github.io/crew/reference/crew_launcher.md).

- `crashes_error`:

  See
  [`crew_launcher()`](https://wlandau.github.io/crew/reference/crew_launcher.md).

- `launch_max`:

  Deprecated.

- `tls`:

  See
  [`crew_launcher()`](https://wlandau.github.io/crew/reference/crew_launcher.md).

- `processes`:

  Deprecated on 2025-08-27 (`crew` version 1.2.1.9009).

- `r_arguments`:

  See
  [`crew_launcher()`](https://wlandau.github.io/crew/reference/crew_launcher.md).

- `options_metrics`:

  See
  [`crew_launcher()`](https://wlandau.github.io/crew/reference/crew_launcher.md).

#### Returns

An `R6` object with the launcher.

#### Examples

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

------------------------------------------------------------------------

### Method `validate()`

Validate the launcher.

#### Usage

    crew_class_launcher$validate()

#### Returns

`NULL` (invisibly).

------------------------------------------------------------------------

### Method `poll()`

Poll the throttle.

#### Usage

    crew_class_launcher$poll()

#### Returns

`TRUE` to run whatever work comes next, `FALSE` to skip until the
appropriate time.

------------------------------------------------------------------------

### Method `settings()`

List of arguments for
[`mirai::daemon()`](https://mirai.r-lib.org/reference/daemon.html).

#### Usage

    crew_class_launcher$settings()

#### Returns

List of arguments for
[`mirai::daemon()`](https://mirai.r-lib.org/reference/daemon.html).

------------------------------------------------------------------------

### Method [`call()`](https://rdrr.io/r/base/call.html)

Create a call to
[`crew_worker()`](https://wlandau.github.io/crew/reference/crew_worker.md)
to help create custom launchers.

#### Usage

    crew_class_launcher$call(worker = NULL)

#### Arguments

- `worker`:

  Deprecated on 2025-08-28 (`crew` version 1.2.1.9009).

#### Returns

Character string with a call to
[`crew_worker()`](https://wlandau.github.io/crew/reference/crew_worker.md).

#### Examples

    launcher <- crew_launcher_local()
    launcher$start(url = "tcp://127.0.0.1:57000", profile = "profile")
    launcher$call()
    launcher$terminate()

------------------------------------------------------------------------

### Method [`start()`](https://rdrr.io/r/stats/start.html)

Start the launcher.

#### Usage

    crew_class_launcher$start(url = NULL, profile = NULL, sockets = NULL)

#### Arguments

- `url`:

  Character string, websocket URL for worker connections.

- `profile`:

  Character string, `mirai` compute profile.

- `sockets`:

  Deprecated on 2025-01-28 (`crew` version 1.0.0).

#### Returns

`NULL` (invisibly).

------------------------------------------------------------------------

### Method `terminate()`

Terminate the whole launcher, including all workers.

#### Usage

    crew_class_launcher$terminate()

#### Returns

`NULL` (invisibly).

------------------------------------------------------------------------

### Method `launch()`

Launch a worker.

#### Usage

    crew_class_launcher$launch(n = 1L)

#### Arguments

- `n`:

  Positive integer, number of workers to launch.

#### Returns

Handle of the launched worker.

------------------------------------------------------------------------

### Method `launch_worker()`

Abstract worker launch method.

#### Usage

    crew_class_launcher$launch_worker(call)

#### Arguments

- `call`:

  Character of length 1 with a namespaced call to
  [`crew_worker()`](https://wlandau.github.io/crew/reference/crew_worker.md)
  which will run in the worker and accept tasks.

#### Details

Launcher plugins will overwrite this method.

#### Returns

A handle to mock the worker launch.

------------------------------------------------------------------------

### Method `launch_workers()`

Launch multiple workers.

#### Usage

    crew_class_launcher$launch_workers(call, n)

#### Arguments

- `call`:

  Character of length 1 with a namespaced call to
  [`crew_worker()`](https://wlandau.github.io/crew/reference/crew_worker.md)
  which will run in each worker and accept tasks.

- `n`:

  Positive integer, number of workers to launch.

#### Details

Launcher plugins may overwrite this method to launch multiple workers
from a single system call.

#### Returns

A handle to mock the worker launch.

------------------------------------------------------------------------

### Method [`scale()`](https://rdrr.io/r/base/scale.html)

Auto-scale workers out to meet the demand of tasks.

#### Usage

    crew_class_launcher$scale(status, throttle = NULL)

#### Arguments

- `status`:

  A `mirai` status list with worker and task information.

- `throttle`:

  Deprecated, only used in the controller as of 2025-01-16 (`crew`
  version 0.10.2.9003).

#### Returns

Invisibly returns `TRUE` if there was any relevant auto-scaling activity
(new worker launches or worker connection/disconnection events) (`FALSE`
otherwise).

------------------------------------------------------------------------

### Method `terminate_workers()`

Deprecated on 2025-08-26 (`crew` version 1.2.1.9004).

#### Usage

    crew_class_launcher$terminate_workers()

#### Returns

`NULL` (invisibly).

------------------------------------------------------------------------

### Method `crashes()`

Deprecated on 2025-01-28 (`crew` version 1.0.0).

#### Usage

    crew_class_launcher$crashes(index = NULL)

#### Arguments

- `index`:

  Unused argument.

#### Returns

The integer 1, for compatibility.

------------------------------------------------------------------------

### Method `set_name()`

Deprecated on 2025-01-28 (`crew` version 1.0.0).

#### Usage

    crew_class_launcher$set_name(name)

#### Arguments

- `name`:

  Name to set for the launcher.

#### Returns

`NULL` (invisibly).

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    crew_class_launcher$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

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

## ------------------------------------------------
## Method `crew_class_launcher$new`
## ------------------------------------------------

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

## ------------------------------------------------
## Method `crew_class_launcher$call`
## ------------------------------------------------

launcher <- crew_launcher_local()
launcher$start(url = "tcp://127.0.0.1:57000", profile = "profile")
launcher$call()
#> [1] "crew::crew_worker(settings = list(url = \"tcp://127.0.0.1:57000\", dispatcher = TRUE, asyncdial = FALSE, autoexit = 15L, cleanup = FALSE, output = TRUE, maxtasks = Inf, idletime = Inf, walltime = Inf, timerstart = 0L, tlscert = NULL, rs = NULL), controller = \"8bf8f7fc\", options_metrics = crew::crew_options_metrics(path = NULL, seconds_interval = 5))"
launcher$terminate()
```
