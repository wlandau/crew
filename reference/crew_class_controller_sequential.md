# Sequential controller class

`R6` class for sequential controllers.

## Details

See
[`crew_controller_sequential()`](https://wlandau.github.io/crew/reference/crew_controller_sequential.md).

## See also

Other sequential controllers:
[`crew_controller_sequential()`](https://wlandau.github.io/crew/reference/crew_controller_sequential.md)

## Super class

[`crew::crew_class_controller`](https://wlandau.github.io/crew/reference/crew_class_controller.md)
-\> `crew_class_controller_sequential`

## Methods

### Public methods

- [`crew_class_controller_sequential$resolved()`](#method-crew_class_controller_sequential-resolved)

- [`crew_class_controller_sequential$unresolved()`](#method-crew_class_controller_sequential-unresolved)

- [`crew_class_controller_sequential$saturated()`](#method-crew_class_controller_sequential-saturated)

- [`crew_class_controller_sequential$start()`](#method-crew_class_controller_sequential-start)

- [`crew_class_controller_sequential$launch()`](#method-crew_class_controller_sequential-launch)

- [`crew_class_controller_sequential$scale()`](#method-crew_class_controller_sequential-scale)

- [`crew_class_controller_sequential$autoscale()`](#method-crew_class_controller_sequential-autoscale)

- [`crew_class_controller_sequential$descale()`](#method-crew_class_controller_sequential-descale)

- [`crew_class_controller_sequential$push()`](#method-crew_class_controller_sequential-push)

- [`crew_class_controller_sequential$wait()`](#method-crew_class_controller_sequential-wait)

- [`crew_class_controller_sequential$push_backlog()`](#method-crew_class_controller_sequential-push_backlog)

- [`crew_class_controller_sequential$pop_backlog()`](#method-crew_class_controller_sequential-pop_backlog)

- [`crew_class_controller_sequential$cancel()`](#method-crew_class_controller_sequential-cancel)

- [`crew_class_controller_sequential$terminate()`](#method-crew_class_controller_sequential-terminate)

Inherited methods

- [`crew::crew_class_controller$collect()`](https://wlandau.github.io/crew/reference/crew_class_controller.html#method-collect)
- [`crew::crew_class_controller$crashes()`](https://wlandau.github.io/crew/reference/crew_class_controller.html#method-crashes)
- [`crew::crew_class_controller$empty()`](https://wlandau.github.io/crew/reference/crew_class_controller.html#method-empty)
- [`crew::crew_class_controller$initialize()`](https://wlandau.github.io/crew/reference/crew_class_controller.html#method-initialize)
- [`crew::crew_class_controller$map()`](https://wlandau.github.io/crew/reference/crew_class_controller.html#method-map)
- [`crew::crew_class_controller$nonempty()`](https://wlandau.github.io/crew/reference/crew_class_controller.html#method-nonempty)
- [`crew::crew_class_controller$pids()`](https://wlandau.github.io/crew/reference/crew_class_controller.html#method-pids)
- [`crew::crew_class_controller$pop()`](https://wlandau.github.io/crew/reference/crew_class_controller.html#method-pop)
- [`crew::crew_class_controller$size()`](https://wlandau.github.io/crew/reference/crew_class_controller.html#method-size)
- [`crew::crew_class_controller$started()`](https://wlandau.github.io/crew/reference/crew_class_controller.html#method-started)
- [`crew::crew_class_controller$summary()`](https://wlandau.github.io/crew/reference/crew_class_controller.html#method-summary)
- [`crew::crew_class_controller$validate()`](https://wlandau.github.io/crew/reference/crew_class_controller.html#method-validate)
- [`crew::crew_class_controller$walk()`](https://wlandau.github.io/crew/reference/crew_class_controller.html#method-walk)

------------------------------------------------------------------------

### Method `resolved()`

Number of resolved tasks.

#### Usage

    crew_class_controller_sequential$resolved(controllers = NULL)

#### Arguments

- `controllers`:

  Not used. Included to ensure the signature is compatible with the
  analogous method of controller groups.

#### Details

`resolved()` is cumulative: it counts all the resolved tasks over the
entire lifetime of the controller session. For the sequential
controller, tasks are resolved as soon as they are pushed.

#### Returns

Non-negative integer of length 1, number of resolved tasks. The return
value is 0 if the condition variable does not exist (i.e. if the client
is not running).

------------------------------------------------------------------------

### Method `unresolved()`

Number of unresolved tasks.

#### Usage

    crew_class_controller_sequential$unresolved(controllers = NULL)

#### Arguments

- `controllers`:

  Not used. Included to ensure the signature is compatible with the
  analogous method of controller groups.

#### Returns

Returns 0 always because the sequential controller resolves tasks as
soon as they are pushed.

------------------------------------------------------------------------

### Method `saturated()`

Check if the controller is saturated.

#### Usage

    crew_class_controller_sequential$saturated(
      collect = NULL,
      throttle = NULL,
      controller = NULL
    )

#### Arguments

- `collect`:

  Deprecated in version 0.5.0.9003 (2023-10-02). Not used.

- `throttle`:

  Deprecated in version 0.5.0.9003 (2023-10-02). Not used.

- `controller`:

  Not used. Included to ensure the signature is compatible with the
  analogous method of controller groups.

#### Returns

Always returns `FALSE` for the sequential controller because tasks run
immediately on the local process and there are no workers.

------------------------------------------------------------------------

### Method [`start()`](https://rdrr.io/r/stats/start.html)

Start the controller if it is not already started.

#### Usage

    crew_class_controller_sequential$start(controllers = NULL)

#### Arguments

- `controllers`:

  Not used. Included to ensure the signature is compatible with the
  analogous method of controller groups.

#### Details

For the sequential controller, there is nothing to do except register
the client as started.

#### Returns

`NULL` (invisibly).

------------------------------------------------------------------------

### Method `launch()`

Does nothing for the sequential controller.

#### Usage

    crew_class_controller_sequential$launch(n = 1L, controllers = NULL)

#### Arguments

- `n`:

  Number of workers to launch.

- `controllers`:

  Not used. Included to ensure the signature is compatible with the
  analogous method of controller groups.

#### Returns

`NULL` (invisibly).

------------------------------------------------------------------------

### Method [`scale()`](https://rdrr.io/r/base/scale.html)

Does nothing for the sequential controller.

#### Usage

    crew_class_controller_sequential$scale(throttle = TRUE, controllers = NULL)

#### Arguments

- `throttle`:

  Not applicable to the sequential controller.

- `controllers`:

  Not used. Included to ensure the signature is compatible with the
  analogous method of controller groups.

#### Returns

Invisibly returns `FALSE`.

------------------------------------------------------------------------

### Method `autoscale()`

Not applicable to the sequential controller.

#### Usage

    crew_class_controller_sequential$autoscale(loop = NULL, controllers = NULL)

#### Arguments

- `loop`:

  Not used by sequential controllers. Included to ensure the signature
  is compatible with the analogous method of controller groups.

- `controllers`:

  Not used by sequential controllers. Included to ensure the signature
  is compatible with the analogous method of controller groups.

------------------------------------------------------------------------

### Method `descale()`

Not applicable to the sequential controller.

#### Usage

    crew_class_controller_sequential$descale(controllers = NULL)

#### Arguments

- `controllers`:

  Not used. Included to ensure the signature is compatible with the
  analogous method of controller groups.

#### Returns

`NULL` (invisibly).

------------------------------------------------------------------------

### Method `push()`

Push a task to the head of the task list.

#### Usage

    crew_class_controller_sequential$push(
      command,
      data = list(),
      globals = list(),
      substitute = TRUE,
      seed = NULL,
      algorithm = NULL,
      packages = character(0),
      library = NULL,
      seconds_timeout = NULL,
      scale = TRUE,
      throttle = TRUE,
      name = NULL,
      save_command = NULL,
      controller = NULL
    )

#### Arguments

- `command`:

  Language object with R code to run.

- `data`:

  Named list of local data objects in the evaluation environment.

- `globals`:

  Named list of objects to temporarily assign to the global environment
  for the task. This list should include any functions you previously
  defined in the global environment which are required to run tasks. See
  the `reset_globals` argument of
  [`crew_controller_local()`](https://wlandau.github.io/crew/reference/crew_controller_local.md).

- `substitute`:

  Logical of length 1, whether to call
  [`base::substitute()`](https://rdrr.io/r/base/substitute.html) on the
  supplied value of the `command` argument. If `TRUE` (default) then
  `command` is quoted literally as you write it, e.g.
  `push(command = your_function_call())`. If `FALSE`, then `crew`
  assumes `command` is a language object and you are passing its value,
  e.g. `push(command = quote(your_function_call()))`.
  `substitute = TRUE` is appropriate for interactive use, whereas
  `substitute = FALSE` is meant for automated R programs that invoke
  `crew` controllers.

- `seed`:

  Integer of length 1 with the pseudo-random number generator seed to
  set for the evaluation of the task. Passed to the `seed` argument of
  [`set.seed()`](https://rdrr.io/r/base/Random.html) if not `NULL`. If
  `algorithm` and `seed` are both `NULL` for the sequential controller,
  then the random number generator defaults to the current RNG of the
  local R session where the sequential controller lives.

- `algorithm`:

  Integer of length 1 with the pseudo-random number generator algorithm
  to set for the evaluation of the task. Passed to the `kind` argument
  of [`RNGkind()`](https://rdrr.io/r/base/Random.html) if not `NULL`. If
  `algorithm` and `seed` are both `NULL` for the sequential controller,
  then the random number generator defaults to the current RNG of the
  local R session where the sequential controller lives.

- `packages`:

  Character vector of packages to load for the task.

- `library`:

  Library path to load the packages. See the `lib.loc` argument of
  [`require()`](https://rdrr.io/r/base/library.html).

- `seconds_timeout`:

  Not used in the sequential controller..

- `scale`:

  Not used in the sequential controller.

- `throttle`:

  Not used in the sequential controller.

- `name`:

  Character string, name of the task. If `NULL`, then a random name is
  generated automatically. The name of the task must not conflict with
  the name of another task pushed to the controller. Any previous task
  with the same name must first be popped before a new task with that
  name can be pushed.

- `save_command`:

  Deprecated on 2025-01-22 (`crew` version 0.10.2.9004) and no longer
  used.

- `controller`:

  Not used. Included to ensure the signature is compatible with the
  analogous method of controller groups.

#### Returns

Invisibly returns a `mirai`-like list where the `data` element is the
result of the task.

------------------------------------------------------------------------

### Method `wait()`

Not applicable to the sequential controller.

#### Usage

    crew_class_controller_sequential$wait(
      mode = "all",
      seconds_interval = NULL,
      seconds_timeout = Inf,
      scale = TRUE,
      throttle = TRUE,
      controllers = NULL
    )

#### Arguments

- `mode`:

  Not applicable to the sequential controller.

- `seconds_interval`:

  Not applicable to the sequential controller.

- `seconds_timeout`:

  Not applicable to the sequential controller.

- `scale`:

  Not applicable to the sequential controller.

- `throttle`:

  Not applicable to the sequential controller.

- `controllers`:

  Not used. Included to ensure the signature is compatible with the
  analogous method of controller groups.

#### Returns

Always returns `TRUE` (invisibly) for the sequential controller.

------------------------------------------------------------------------

### Method `push_backlog()`

Not applicable to the sequential controller.

#### Usage

    crew_class_controller_sequential$push_backlog(name, controller = NULL)

#### Arguments

- `name`:

  Character of length 1 with the task name to push to the backlog.

- `controller`:

  Not used. Included to ensure the signature is compatible with the
  analogous method of controller groups.

#### Returns

`NULL` (invisibly).

------------------------------------------------------------------------

### Method `pop_backlog()`

Not applicable to the sequential controller.

#### Usage

    crew_class_controller_sequential$pop_backlog(controllers = NULL)

#### Arguments

- `controllers`:

  Not used. Included to ensure the signature is compatible with the
  analogous method of controller groups.

#### Returns

Always `character(0L)` for the sequential controller.

------------------------------------------------------------------------

### Method `cancel()`

Not applicable to the sequential controller.

#### Usage

    crew_class_controller_sequential$cancel(names = character(0L), all = FALSE)

#### Arguments

- `names`:

  Not applicable to the sequential controller.

- `all`:

  Not applicable to the sequential controller.

------------------------------------------------------------------------

### Method `terminate()`

Terminate the controller.

#### Usage

    crew_class_controller_sequential$terminate(controllers = NULL)

#### Arguments

- `controllers`:

  Not used. Included to ensure the signature is compatible with the
  analogous method of controller groups.

#### Returns

`NULL` (invisibly).

## Examples

``` r
if (identical(Sys.getenv("CREW_EXAMPLES"), "true")) {
controller <- crew_controller_sequential()
controller$push(name = "task", command = sqrt(4))
controller$pop()
}
```
