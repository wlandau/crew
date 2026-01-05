# Local process launcher class

`R6` class to launch and manage local process workers.

## Details

See
[`crew_launcher_local()`](https://wlandau.github.io/crew/reference/crew_launcher_local.md).

## See also

Other plugin_local:
[`crew_controller_local()`](https://wlandau.github.io/crew/reference/crew_controller_local.md),
[`crew_launcher_local()`](https://wlandau.github.io/crew/reference/crew_launcher_local.md)

## Super class

[`crew::crew_class_launcher`](https://wlandau.github.io/crew/reference/crew_class_launcher.md)
-\> `crew_class_launcher_local`

## Active bindings

- `options_local`:

  See
  [`crew_launcher_local()`](https://wlandau.github.io/crew/reference/crew_launcher_local.md).

## Methods

### Public methods

- [`crew_class_launcher_local$new()`](#method-crew_class_launcher_local-new)

- [`crew_class_launcher_local$validate()`](#method-crew_class_launcher_local-validate)

- [`crew_class_launcher_local$launch_worker()`](#method-crew_class_launcher_local-launch_worker)

- [`crew_class_launcher_local$clone()`](#method-crew_class_launcher_local-clone)

Inherited methods

- [`crew::crew_class_launcher$call()`](https://wlandau.github.io/crew/reference/crew_class_launcher.html#method-call)
- [`crew::crew_class_launcher$crashes()`](https://wlandau.github.io/crew/reference/crew_class_launcher.html#method-crashes)
- [`crew::crew_class_launcher$launch()`](https://wlandau.github.io/crew/reference/crew_class_launcher.html#method-launch)
- [`crew::crew_class_launcher$launch_workers()`](https://wlandau.github.io/crew/reference/crew_class_launcher.html#method-launch_workers)
- [`crew::crew_class_launcher$poll()`](https://wlandau.github.io/crew/reference/crew_class_launcher.html#method-poll)
- [`crew::crew_class_launcher$scale()`](https://wlandau.github.io/crew/reference/crew_class_launcher.html#method-scale)
- [`crew::crew_class_launcher$set_name()`](https://wlandau.github.io/crew/reference/crew_class_launcher.html#method-set_name)
- [`crew::crew_class_launcher$settings()`](https://wlandau.github.io/crew/reference/crew_class_launcher.html#method-settings)
- [`crew::crew_class_launcher$start()`](https://wlandau.github.io/crew/reference/crew_class_launcher.html#method-start)
- [`crew::crew_class_launcher$terminate()`](https://wlandau.github.io/crew/reference/crew_class_launcher.html#method-terminate)
- [`crew::crew_class_launcher$terminate_workers()`](https://wlandau.github.io/crew/reference/crew_class_launcher.html#method-terminate_workers)

------------------------------------------------------------------------

### Method `new()`

Local launcher constructor.

#### Usage

    crew_class_launcher_local$new(
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
      crashes_error = NULL,
      tls = NULL,
      processes = NULL,
      r_arguments = NULL,
      options_metrics = NULL,
      options_local = NULL
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

- `crashes_error`:

  See
  [`crew_launcher()`](https://wlandau.github.io/crew/reference/crew_launcher.md).

- `tls`:

  See
  [`crew_launcher()`](https://wlandau.github.io/crew/reference/crew_launcher.md).

- `processes`:

  See
  [`crew_launcher()`](https://wlandau.github.io/crew/reference/crew_launcher.md).

- `r_arguments`:

  See
  [`crew_launcher()`](https://wlandau.github.io/crew/reference/crew_launcher.md).

- `options_metrics`:

  See
  [`crew_launcher_local()`](https://wlandau.github.io/crew/reference/crew_launcher_local.md).

- `options_local`:

  See
  [`crew_launcher_local()`](https://wlandau.github.io/crew/reference/crew_launcher_local.md).

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

#### Returns

An `R6` object with the local launcher.

#### Examples

    if (identical(Sys.getenv("CREW_EXAMPLES"), "true")) {
    client <- crew_client()
    client$start()
    launcher <- crew_launcher_local(name = client$name)
    launcher$start(url = client$url, profile = client$profile)
    launcher$launch()
    task <- mirai::mirai("result", .compute = client$profile)
    mirai::call_mirai(task)
    task$data
    client$terminate()
    }

------------------------------------------------------------------------

### Method `validate()`

Validate the local launcher.

#### Usage

    crew_class_launcher_local$validate()

#### Returns

`NULL` (invisibly).

------------------------------------------------------------------------

### Method `launch_worker()`

Launch a local process worker which will dial into a socket.

#### Usage

    crew_class_launcher_local$launch_worker(call)

#### Arguments

- `call`:

  Character of length 1 with a namespaced call to
  [`crew_worker()`](https://wlandau.github.io/crew/reference/crew_worker.md)
  which will run in the worker and accept tasks.

#### Details

The `call` argument is R code that will run to initiate the worker.
Together, the `launcher`, `worker`, and `instance` arguments are useful
for constructing informative job names.

#### Returns

A handle object to allow the termination of the worker later on.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    crew_class_launcher_local$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
if (identical(Sys.getenv("CREW_EXAMPLES"), "true")) {
client <- crew_client()
client$start()
launcher <- crew_launcher_local(name = client$name)
launcher$start(url = client$url, profile = client$profile)
launcher$launch()
task <- mirai::mirai("result", .compute = client$profile)
mirai::call_mirai(task)
task$data
client$terminate()
}

## ------------------------------------------------
## Method `crew_class_launcher_local$new`
## ------------------------------------------------

if (identical(Sys.getenv("CREW_EXAMPLES"), "true")) {
client <- crew_client()
client$start()
launcher <- crew_launcher_local(name = client$name)
launcher$start(url = client$url, profile = client$profile)
launcher$launch()
task <- mirai::mirai("result", .compute = client$profile)
mirai::call_mirai(task)
task$data
client$terminate()
}
```
