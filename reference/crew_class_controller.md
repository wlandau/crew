# Controller class

`R6` class for controllers.

## Details

See
[`crew_controller()`](https://wlandau.github.io/crew/reference/crew_controller.md).

## See also

Other controller:
[`crew_controller()`](https://wlandau.github.io/crew/reference/crew_controller.md)

## Active bindings

- `profile`:

  Character string, compute profile of the controller.

- `client`:

  Client object.

- `launcher`:

  Launcher object.

- `tasks`:

  A list of
  [`mirai::mirai()`](https://mirai.r-lib.org/reference/mirai.html) task
  objects. The list of tasks is dynamically generated from an internal,
  dictionary, so it is not as fast as a simple lookup.

- `reset_globals`:

  See
  [`crew_controller()`](https://wlandau.github.io/crew/reference/crew_controller.md).
  since the controller was started.

- `reset_packages`:

  See
  [`crew_controller()`](https://wlandau.github.io/crew/reference/crew_controller.md).
  since the controller was started.

- `reset_options`:

  See
  [`crew_controller()`](https://wlandau.github.io/crew/reference/crew_controller.md).
  since the controller was started.

- `garbage_collection`:

  See
  [`crew_controller()`](https://wlandau.github.io/crew/reference/crew_controller.md).
  since the controller was started.

- `crashes_max`:

  See
  [`crew_controller()`](https://wlandau.github.io/crew/reference/crew_controller.md).

- `backup`:

  See
  [`crew_controller()`](https://wlandau.github.io/crew/reference/crew_controller.md).

- `error`:

  Tibble of task results (with one result per row) from the last call to
  `map(error = "stop)`.

- `loop`:

  `later` loop if asynchronous auto-scaling is running, `NULL`
  otherwise.

- `queue_resolved`:

  Queue of resolved tasks.

- `queue_backlog`:

  Queue of explicitly backlogged tasks.

## Methods

### Public methods

- [`crew_class_controller$new()`](#method-crew_class_controller-new)

- [`crew_class_controller$validate()`](#method-crew_class_controller-validate)

- [`crew_class_controller$size()`](#method-crew_class_controller-size)

- [`crew_class_controller$empty()`](#method-crew_class_controller-empty)

- [`crew_class_controller$nonempty()`](#method-crew_class_controller-nonempty)

- [`crew_class_controller$resolved()`](#method-crew_class_controller-resolved)

- [`crew_class_controller$unresolved()`](#method-crew_class_controller-unresolved)

- [`crew_class_controller$saturated()`](#method-crew_class_controller-saturated)

- [`crew_class_controller$start()`](#method-crew_class_controller-start)

- [`crew_class_controller$started()`](#method-crew_class_controller-started)

- [`crew_class_controller$launch()`](#method-crew_class_controller-launch)

- [`crew_class_controller$scale()`](#method-crew_class_controller-scale)

- [`crew_class_controller$autoscale()`](#method-crew_class_controller-autoscale)

- [`crew_class_controller$descale()`](#method-crew_class_controller-descale)

- [`crew_class_controller$crashes()`](#method-crew_class_controller-crashes)

- [`crew_class_controller$push()`](#method-crew_class_controller-push)

- [`crew_class_controller$walk()`](#method-crew_class_controller-walk)

- [`crew_class_controller$map()`](#method-crew_class_controller-map)

- [`crew_class_controller$pop()`](#method-crew_class_controller-pop)

- [`crew_class_controller$collect()`](#method-crew_class_controller-collect)

- [`crew_class_controller$wait()`](#method-crew_class_controller-wait)

- [`crew_class_controller$push_backlog()`](#method-crew_class_controller-push_backlog)

- [`crew_class_controller$pop_backlog()`](#method-crew_class_controller-pop_backlog)

- [`crew_class_controller$summary()`](#method-crew_class_controller-summary)

- [`crew_class_controller$cancel()`](#method-crew_class_controller-cancel)

- [`crew_class_controller$pids()`](#method-crew_class_controller-pids)

- [`crew_class_controller$terminate()`](#method-crew_class_controller-terminate)

------------------------------------------------------------------------

### Method `new()`

`mirai` controller constructor.

#### Usage

    crew_class_controller$new(
      client = NULL,
      launcher = NULL,
      reset_globals = NULL,
      reset_packages = NULL,
      reset_options = NULL,
      garbage_collection = NULL,
      crashes_max = NULL,
      backup = NULL
    )

#### Arguments

- `client`:

  Client object. See
  [`crew_controller()`](https://wlandau.github.io/crew/reference/crew_controller.md).

- `launcher`:

  Launcher object. See
  [`crew_controller()`](https://wlandau.github.io/crew/reference/crew_controller.md).

- `reset_globals`:

  See
  [`crew_controller()`](https://wlandau.github.io/crew/reference/crew_controller.md).

- `reset_packages`:

  See
  [`crew_controller()`](https://wlandau.github.io/crew/reference/crew_controller.md).

- `reset_options`:

  See
  [`crew_controller()`](https://wlandau.github.io/crew/reference/crew_controller.md).

- `garbage_collection`:

  See
  [`crew_controller()`](https://wlandau.github.io/crew/reference/crew_controller.md).

- `crashes_max`:

  See
  [`crew_controller()`](https://wlandau.github.io/crew/reference/crew_controller.md).

- `backup`:

  See
  [`crew_controller()`](https://wlandau.github.io/crew/reference/crew_controller.md).

#### Returns

An `R6` controller object.

#### Examples

    if (identical(Sys.getenv("CREW_EXAMPLES"), "true")) {
    client <- crew_client()
    launcher <- crew_launcher_local()
    controller <- crew_controller(client = client, launcher = launcher)
    controller$start()
    controller$push(name = "task", command = sqrt(4))
    controller$wait()
    controller$pop()
    controller$terminate()
    }

------------------------------------------------------------------------

### Method `validate()`

Validate the controller.

#### Usage

    crew_class_controller$validate()

#### Returns

`NULL` (invisibly).

------------------------------------------------------------------------

### Method `size()`

Number of tasks in the controller.

#### Usage

    crew_class_controller$size(controllers = NULL)

#### Arguments

- `controllers`:

  Not used. Included to ensure the signature is compatible with the
  analogous method of controller groups.

#### Returns

Non-negative integer, number of tasks in the controller.

------------------------------------------------------------------------

### Method `empty()`

Check if the controller is empty.

#### Usage

    crew_class_controller$empty(controllers = NULL)

#### Arguments

- `controllers`:

  Not used. Included to ensure the signature is compatible with the
  analogous method of controller groups.

#### Details

A controller is empty if it has no
[`mirai::mirai()`](https://mirai.r-lib.org/reference/mirai.html) task
objects in the controller. There may still be other tasks running on the
workers of an empty controller, but those tasks were not submitted with
`push()` or `collect()`, and they are not part of the controller task
queue.

#### Returns

`TRUE` if the controller is empty, `FALSE` otherwise.

------------------------------------------------------------------------

### Method `nonempty()`

Check if the controller is nonempty.

#### Usage

    crew_class_controller$nonempty(controllers = NULL)

#### Arguments

- `controllers`:

  Not used. Included to ensure the signature is compatible with the
  analogous method of controller groups.

#### Details

A controller is empty if it has no
[`mirai::mirai()`](https://mirai.r-lib.org/reference/mirai.html) task
objects in the controller. There may still be other tasks running on the
workers of an empty controller, but those tasks were not submitted with
`push()` or `collect()`, and they are not part of the controller task
queue.

#### Returns

`TRUE` if the controller is empty, `FALSE` otherwise.

------------------------------------------------------------------------

### Method `resolved()`

Cumulative number of resolved tasks.

#### Usage

    crew_class_controller$resolved(controllers = NULL)

#### Arguments

- `controllers`:

  Not used. Included to ensure the signature is compatible with the
  analogous method of controller groups.

#### Details

`resolved()` is cumulative: it counts all the resolved tasks over the
entire lifetime of the controller session.

#### Returns

Non-negative integer of length 1, number of resolved tasks. The return
value is 0 if the condition variable does not exist (i.e. if the client
is not running).

------------------------------------------------------------------------

### Method `unresolved()`

Number of unresolved tasks.

#### Usage

    crew_class_controller$unresolved(controllers = NULL)

#### Arguments

- `controllers`:

  Not used. Included to ensure the signature is compatible with the
  analogous method of controller groups.

#### Returns

Non-negative integer of length 1, number of unresolved tasks.

------------------------------------------------------------------------

### Method `saturated()`

Check if the controller is saturated.

#### Usage

    crew_class_controller$saturated(
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

#### Details

A controller is saturated if the number of uncollected tasks is greater
than or equal to the maximum number of workers. You can still push tasks
to a saturated controller, but tools that use `crew` such as `targets`
may choose not to (for performance and user-friendliness).

#### Returns

`TRUE` if the controller is saturated, `FALSE` otherwise.

------------------------------------------------------------------------

### Method [`start()`](https://rdrr.io/r/stats/start.html)

Start the controller if it is not already started.

#### Usage

    crew_class_controller$start(controllers = NULL)

#### Arguments

- `controllers`:

  Not used. Included to ensure the signature is compatible with the
  analogous method of controller groups.

#### Details

Register the mirai client and register worker websockets with the
launcher.

#### Returns

`NULL` (invisibly).

------------------------------------------------------------------------

### Method `started()`

Check whether the controller is started.

#### Usage

    crew_class_controller$started(controllers = NULL)

#### Arguments

- `controllers`:

  Not used. Included to ensure the signature is compatible with the
  analogous method of controller groups.

#### Details

Actually checks whether the client is started.

#### Returns

`TRUE` if the controller is started, `FALSE` otherwise.

------------------------------------------------------------------------

### Method `launch()`

Launch one or more workers.

#### Usage

    crew_class_controller$launch(n = 1L, controllers = NULL)

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

Auto-scale workers out to meet the demand of tasks.

#### Usage

    crew_class_controller$scale(throttle = TRUE, controllers = NULL)

#### Arguments

- `throttle`:

  `TRUE` to skip auto-scaling if it already happened within the last
  polling interval. `FALSE` to auto-scale every time
  [`scale()`](https://rdrr.io/r/base/scale.html) is called. Throttling
  avoids overburdening the `mirai` dispatcher and other resources.

- `controllers`:

  Not used. Included to ensure the signature is compatible with the
  analogous method of controller groups.

#### Details

The [`scale()`](https://rdrr.io/r/base/scale.html) method launches new
workers to run tasks if needed.

#### Returns

Invisibly returns `TRUE` if auto-scaling was attempted (throttling can
skip it) and there was any relevant auto-scaling activity (new worker
launches or worker connection/disconnection events). `FALSE` otherwise.

------------------------------------------------------------------------

### Method `autoscale()`

Run worker auto-scaling in a `later` loop in polling intervals
determined by exponential backoff.

#### Usage

    crew_class_controller$autoscale(
      loop = later::global_loop(),
      controllers = NULL
    )

#### Arguments

- `loop`:

  A `later` loop to run auto-scaling.

- `controllers`:

  Not used. Included to ensure the signature is compatible with the
  analogous method of controller groups.

#### Details

Call `controller$descale()` to terminate the auto-scaling loop.

#### Returns

`NULL` (invisibly).

------------------------------------------------------------------------

### Method `descale()`

Terminate the auto-scaling loop started by `controller$autoscale()`.

#### Usage

    crew_class_controller$descale(controllers = NULL)

#### Arguments

- `controllers`:

  Not used. Included to ensure the signature is compatible with the
  analogous method of controller groups.

#### Returns

`NULL` (invisibly).

------------------------------------------------------------------------

### Method `crashes()`

Report the number of consecutive crashes of a task.

#### Usage

    crew_class_controller$crashes(name, controllers = NULL)

#### Arguments

- `name`:

  Character string, name of the task to check.

- `controllers`:

  Not used. Included to ensure the signature is compatible with the
  analogous method of controller groups.

#### Details

See the `crashes_max` argument of
[`crew_controller()`](https://wlandau.github.io/crew/reference/crew_controller.md).

#### Returns

Non-negative integer, number of consecutive times the task crashed.

------------------------------------------------------------------------

### Method `push()`

Push a task to the head of the task list.

#### Usage

    crew_class_controller$push(
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
  `algorithm` and `seed` are both `NULL`, then the random number
  generator defaults to the widely spaced worker-specific L'Ecuyer
  streams as supported by
  [`mirai::nextstream()`](https://mirai.r-lib.org/reference/nextstream.html).
  See
  [`vignette("parallel", package = "parallel")`](https://cran.rstudio.com/web/packages/parallel/vignettes/parallel.pdf)
  for details.

- `algorithm`:

  Integer of length 1 with the pseudo-random number generator algorithm
  to set for the evaluation of the task. Passed to the `kind` argument
  of [`RNGkind()`](https://rdrr.io/r/base/Random.html) if not `NULL`. If
  `algorithm` and `seed` are both `NULL`, then the random number
  generator defaults to the recommended widely spaced worker-specific
  L'Ecuyer streams as supported by
  [`mirai::nextstream()`](https://mirai.r-lib.org/reference/nextstream.html).
  See
  [`vignette("parallel", package = "parallel")`](https://cran.rstudio.com/web/packages/parallel/vignettes/parallel.pdf)
  for details.

- `packages`:

  Character vector of packages to load for the task.

- `library`:

  Library path to load the packages. See the `lib.loc` argument of
  [`require()`](https://rdrr.io/r/base/library.html).

- `seconds_timeout`:

  Optional task timeout passed to the `.timeout` argument of
  [`mirai::mirai()`](https://mirai.r-lib.org/reference/mirai.html)
  (after converting to milliseconds).

- `scale`:

  Logical, whether to automatically call
  [`scale()`](https://rdrr.io/r/base/scale.html) to auto-scale workers
  to meet the demand of the task load. Also see the `throttle` argument.

- `throttle`:

  `TRUE` to skip auto-scaling if it already happened within the last
  polling interval. `FALSE` to auto-scale every time
  [`scale()`](https://rdrr.io/r/base/scale.html) is called. Throttling
  avoids overburdening the `mirai` dispatcher and other resources.

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

Invisibly return the `mirai` object of the pushed task. This allows you
to interact with the task directly, e.g. to create a promise object with
[`promises::as.promise()`](https://rstudio.github.io/promises/reference/is.promise.html).

------------------------------------------------------------------------

### Method `walk()`

Apply a single command to multiple inputs, and return control to the
user without waiting for any task to complete.

#### Usage

    crew_class_controller$walk(
      command,
      iterate,
      data = list(),
      globals = list(),
      substitute = TRUE,
      seed = NULL,
      algorithm = NULL,
      packages = character(0),
      library = NULL,
      seconds_timeout = NULL,
      names = NULL,
      save_command = NULL,
      verbose = interactive(),
      scale = TRUE,
      throttle = TRUE,
      controller = NULL
    )

#### Arguments

- `command`:

  Language object with R code to run.

- `iterate`:

  Named list of vectors or lists to iterate over. For example, to run
  function calls `f(x = 1, y = "a")` and `f(x = 2, y = "b")`, set
  `command` to `f(x, y)`, and set `iterate` to
  `list(x = c(1, 2), y = c("a", "b"))`. The individual function calls
  are evaluated as `f(x = iterate$x[[1]], y = iterate$y[[1]])` and
  `f(x = iterate$x[[2]], y = iterate$y[[2]])`. All the elements of
  `iterate` must have the same length. If there are any name conflicts
  between `iterate` and `data`, `iterate` takes precedence.

- `data`:

  Named list of constant local data objects in the evaluation
  environment. Objects in this list are treated as single values and are
  held constant for each iteration of the map.

- `globals`:

  Named list of constant objects to temporarily assign to the global
  environment for each task. This list should include any functions you
  previously defined in the global environment which are required to run
  tasks. See the `reset_globals` argument of
  [`crew_controller_local()`](https://wlandau.github.io/crew/reference/crew_controller_local.md).
  Objects in this list are treated as single values and are held
  constant for each iteration of the map.

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
  `algorithm` and `seed` are both `NULL`, then the random number
  generator defaults to the recommended widely spaced worker-specific
  L'Ecuyer streams as supported by
  [`mirai::nextstream()`](https://mirai.r-lib.org/reference/nextstream.html).
  See
  [`vignette("parallel", package = "parallel")`](https://cran.rstudio.com/web/packages/parallel/vignettes/parallel.pdf)
  for details.

- `algorithm`:

  Integer of length 1 with the pseudo-random number generator algorithm
  to set for the evaluation of the task. Passed to the `kind` argument
  of [`RNGkind()`](https://rdrr.io/r/base/Random.html) if not `NULL`. If
  `algorithm` and `seed` are both `NULL`, then the random number
  generator defaults to the recommended widely spaced worker-specific
  L'Ecuyer streams as supported by
  [`mirai::nextstream()`](https://mirai.r-lib.org/reference/nextstream.html).
  See
  [`vignette("parallel", package = "parallel")`](https://cran.rstudio.com/web/packages/parallel/vignettes/parallel.pdf)
  for details.

- `packages`:

  Character vector of packages to load for the task.

- `library`:

  Library path to load the packages. See the `lib.loc` argument of
  [`require()`](https://rdrr.io/r/base/library.html).

- `seconds_timeout`:

  Optional task timeout passed to the `.timeout` argument of
  [`mirai::mirai()`](https://mirai.r-lib.org/reference/mirai.html)
  (after converting to milliseconds).

- `names`:

  Optional character of length 1, name of the element of `iterate` with
  names for the tasks. If `names` is supplied, then `iterate[[names]]`
  must be a character vector.

- `save_command`:

  Deprecated on 2025-01-22 (`crew` version 0.10.2.9004). The command is
  always saved now.

- `verbose`:

  Logical of length 1, whether to print to a progress bar when pushing
  tasks.

- `scale`:

  Logical, whether to automatically scale workers to meet demand. See
  also the `throttle` argument.

- `throttle`:

  `TRUE` to skip auto-scaling if it already happened within the last
  polling interval. `FALSE` to auto-scale every time
  [`scale()`](https://rdrr.io/r/base/scale.html) is called. Throttling
  avoids overburdening the `mirai` dispatcher and other resources.

- `controller`:

  Not used. Included to ensure the signature is compatible with the
  analogous method of controller groups.

#### Details

In contrast to `walk()`, `map()` blocks the local R session and waits
for all tasks to complete.

#### Returns

Invisibly returns a list of `mirai` task objects for the newly created
tasks. The order of tasks in the list matches the order of data in the
`iterate` argument.

------------------------------------------------------------------------

### Method `map()`

Apply a single command to multiple inputs, wait for all tasks to
complete, and return the results of all tasks.

#### Usage

    crew_class_controller$map(
      command,
      iterate,
      data = list(),
      globals = list(),
      substitute = TRUE,
      seed = NULL,
      algorithm = NULL,
      packages = character(0),
      library = NULL,
      seconds_interval = NULL,
      seconds_timeout = NULL,
      names = NULL,
      save_command = NULL,
      error = "stop",
      warnings = TRUE,
      verbose = interactive(),
      scale = TRUE,
      throttle = TRUE,
      controller = NULL
    )

#### Arguments

- `command`:

  Language object with R code to run.

- `iterate`:

  Named list of vectors or lists to iterate over. For example, to run
  function calls `f(x = 1, y = "a")` and `f(x = 2, y = "b")`, set
  `command` to `f(x, y)`, and set `iterate` to
  `list(x = c(1, 2), y = c("a", "b"))`. The individual function calls
  are evaluated as `f(x = iterate$x[[1]], y = iterate$y[[1]])` and
  `f(x = iterate$x[[2]], y = iterate$y[[2]])`. All the elements of
  `iterate` must have the same length. If there are any name conflicts
  between `iterate` and `data`, `iterate` takes precedence.

- `data`:

  Named list of constant local data objects in the evaluation
  environment. Objects in this list are treated as single values and are
  held constant for each iteration of the map.

- `globals`:

  Named list of constant objects to temporarily assign to the global
  environment for each task. This list should include any functions you
  previously defined in the global environment which are required to run
  tasks. See the `reset_globals` argument of
  [`crew_controller_local()`](https://wlandau.github.io/crew/reference/crew_controller_local.md).
  Objects in this list are treated as single values and are held
  constant for each iteration of the map.

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
  `algorithm` and `seed` are both `NULL`, then the random number
  generator defaults to the recommended widely spaced worker-specific
  L'Ecuyer streams as supported by
  [`mirai::nextstream()`](https://mirai.r-lib.org/reference/nextstream.html).
  See
  [`vignette("parallel", package = "parallel")`](https://cran.rstudio.com/web/packages/parallel/vignettes/parallel.pdf)
  for details.

- `algorithm`:

  Integer of length 1 with the pseudo-random number generator algorithm
  to set for the evaluation of the task. Passed to the `kind` argument
  of [`RNGkind()`](https://rdrr.io/r/base/Random.html) if not `NULL`. If
  `algorithm` and `seed` are both `NULL`, then the random number
  generator defaults to the recommended widely spaced worker-specific
  L'Ecuyer streams as supported by
  [`mirai::nextstream()`](https://mirai.r-lib.org/reference/nextstream.html).
  See
  [`vignette("parallel", package = "parallel")`](https://cran.rstudio.com/web/packages/parallel/vignettes/parallel.pdf)
  for details.

- `packages`:

  Character vector of packages to load for the task.

- `library`:

  Library path to load the packages. See the `lib.loc` argument of
  [`require()`](https://rdrr.io/r/base/library.html).

- `seconds_interval`:

  Deprecated on 2025-01-17 (`crew` version 0.10.2.9003). Instead, the
  `seconds_interval` argument passed to
  [`crew_controller_group()`](https://wlandau.github.io/crew/reference/crew_controller_group.md)
  is used as `seconds_max` in a
  [`crew_throttle()`](https://wlandau.github.io/crew/reference/crew_throttle.md)
  object which orchestrates exponential backoff.

- `seconds_timeout`:

  Optional task timeout passed to the `.timeout` argument of
  [`mirai::mirai()`](https://mirai.r-lib.org/reference/mirai.html)
  (after converting to milliseconds).

- `names`:

  Optional character string, name of the element of `iterate` with names
  for the tasks. If `names` is supplied, then `iterate[[names]]` must be
  a character vector.

- `save_command`:

  Deprecated on 2025-01-22 (`crew` version 0.10.2.9004). The command is
  always saved now.

- `error`:

  Character of length 1, choice of action if a task was not successful.
  Possible values:

  - `"stop"`: throw an error in the main R session instead of returning
    a value. In case of an error, the results from the last errored
    `map()` are in the `error` field of the controller, e.g.
    `controller_object$error`. To reduce memory consumption, set
    `controller_object$error <- NULL` after you are finished
    troubleshooting.

  - `"warn"`: throw a warning. This allows the return value with all the
    error messages and tracebacks to be generated.

  - `"silent"`: do nothing special. NOTE: the only kinds of errors
    considered here are errors at the R level. A crashed tasks will
    return a status of `"crash"` in the output and not trigger an error
    in `map()` unless `crashes_max` is reached.

- `warnings`:

  Logical of length 1, whether to throw a warning in the interactive
  session if at least one task encounters an error.

- `verbose`:

  Logical of length 1, whether to print to a progress bar as tasks
  resolve.

- `scale`:

  Logical, whether to automatically scale workers to meet demand. See
  also the `throttle` argument.

- `throttle`:

  `TRUE` to skip auto-scaling if it already happened within the last
  polling interval. `FALSE` to auto-scale every time
  [`scale()`](https://rdrr.io/r/base/scale.html) is called. Throttling
  avoids overburdening the `mirai` dispatcher and other resources.

- `controller`:

  Not used. Included to ensure the signature is compatible with the
  analogous method of controller groups.

#### Details

`map()` cannot be used unless all prior tasks are completed and popped.
You may need to wait and then pop them manually. Alternatively, you can
start over: either call `terminate()` on the current controller object
to reset it, or create a new controller object entirely.

#### Returns

A `tibble` of results and metadata: one row per task and columns
corresponding to the output of `pop()`.

------------------------------------------------------------------------

### Method `pop()`

Pop a completed task from the results data frame.

#### Usage

    crew_class_controller$pop(
      scale = TRUE,
      collect = NULL,
      throttle = TRUE,
      error = NULL,
      controllers = NULL
    )

#### Arguments

- `scale`:

  Logical of length 1, whether to automatically call
  [`scale()`](https://rdrr.io/r/base/scale.html) to auto-scale workers
  to meet the demand of the task load. Scaling up on `pop()` may be
  important for transient or nearly transient workers that tend to drop
  off quickly after doing little work. See also the `throttle` argument.

- `collect`:

  Deprecated in version 0.5.0.9003 (2023-10-02).

- `throttle`:

  `TRUE` to skip auto-scaling if it already happened within the last
  polling interval. `FALSE` to auto-scale every time
  [`scale()`](https://rdrr.io/r/base/scale.html) is called. Throttling
  avoids overburdening the `mirai` dispatcher and other resources.

- `error`:

  `NULL` or character of length 1, choice of action if the popped task
  threw an error. Possible values:

  - `"stop"`: throw an error in the main R session instead of returning
    a value.

  - `"warn"`: throw a warning.

  - `NULL` or `"silent"`: do not react to errors. NOTE: the only kinds
    of errors considered here are errors at the R level. A crashed tasks
    will return a status of `"crash"` in the output and not trigger an
    error in `pop()` unless `crashes_max` is reached.

- `controllers`:

  Not used. Included to ensure the signature is compatible with the
  analogous method of controller groups.

#### Details

If not task is currently completed, `pop()` will attempt to auto-scale
workers as needed.

#### Returns

If there is no task to collect, return `NULL`. Otherwise, return a
one-row `tibble` with the following columns.

- `name`: the task name.

- `command`: a character string with the R command.

- `result`: a list containing the return value of the R command. `NA` if
  the task failed.

- `status`: a character string. `"success"` if the task succeeded,
  `"cancel"` if the task was canceled with the `cancel()` controller
  method, `"crash"` if the worker running the task exited before it
  could complete the task, or `"error"` for any other kind of error.

- `error`: the first 2048 characters of the error message if the task
  status is not `"success"`, `NA` otherwise. Messages for crashes and
  cancellations are captured here alongside ordinary R-level errors.

- `code`: an integer code denoting the specific exit status: `0` for
  successful tasks, `-1` for tasks with an error in the R command of the
  task, and another positive integer with an NNG status code if there is
  an error at the NNG/`nanonext` level.
  [`nanonext::nng_error()`](https://nanonext.r-lib.org/reference/nng_error.html)
  can interpret these codes.

- `trace`: the first 2048 characters of the text of the traceback if the
  task threw an error, `NA` otherwise.

- `warnings`: the first 2048 characters. of the text of warning messages
  that the task may have generated, `NA` otherwise.

- `seconds`: number of seconds that the task ran.

- `seed`: the single integer originally supplied to `push()`, `NA`
  otherwise. The pseudo-random number generator state just prior to the
  task can be restored using `set.seed(seed = seed, kind = algorithm)`,
  where `seed` and `algorithm` are part of this output.

- `algorithm`: name of the pseudo-random number generator algorithm
  originally supplied to `push()`, `NA` otherwise. The pseudo-random
  number generator state just prior to the task can be restored using
  `set.seed(seed = seed, kind = algorithm)`, where `seed` and
  `algorithm` are part of this output.

- `controller`: name of the `crew` controller where the task ran.

- `worker`: name of the `crew` worker that ran the task.

------------------------------------------------------------------------

### Method `collect()`

Pop all available task results and return them in a tidy `tibble`.

#### Usage

    crew_class_controller$collect(
      scale = TRUE,
      throttle = TRUE,
      error = NULL,
      controllers = NULL
    )

#### Arguments

- `scale`:

  Logical of length 1, whether to automatically call
  [`scale()`](https://rdrr.io/r/base/scale.html) to auto-scale workers
  to meet the demand of the task load.

- `throttle`:

  `TRUE` to skip auto-scaling if it already happened within the last
  polling interval. `FALSE` to auto-scale every time
  [`scale()`](https://rdrr.io/r/base/scale.html) is called. Throttling
  avoids overburdening the `mirai` dispatcher and other resources.

- `error`:

  `NULL` or character of length 1, choice of action if the popped task
  threw an error. Possible values: \* `"stop"`: throw an error in the
  main R session instead of returning a value. \* `"warn"`: throw a
  warning. \* `NULL` or `"silent"`: do not react to errors. NOTE: the
  only kinds of errors considered here are errors at the R level. A
  crashed tasks will return a status of `"crash"` in the output and not
  trigger an error in `collect()` unless `crashes_max` is reached.

- `controllers`:

  Not used. Included to ensure the signature is compatible with the
  analogous method of controller groups.

#### Returns

A `tibble` of results and metadata of all resolved tasks, with one row
per task. Returns `NULL` if there are no tasks to collect. See `pop()`
for details on the columns of the returned `tibble`.

------------------------------------------------------------------------

### Method `wait()`

Wait for tasks.

#### Usage

    crew_class_controller$wait(
      mode = "all",
      seconds_interval = NULL,
      seconds_timeout = Inf,
      scale = TRUE,
      throttle = TRUE,
      controllers = NULL
    )

#### Arguments

- `mode`:

  Character string, name of the waiting condition. `wait(mode = "all")`
  waits until all tasks in the `mirai` compute profile resolve, and
  `wait(mode = "one")` waits until at least one task is available to
  `push()` or `collect()` from the controller. The former still works if
  the controller is not the only means of submitting tasks to the
  compute profile, whereas the latter assumes only the controller
  submits tasks.

- `seconds_interval`:

  Deprecated on 2025-01-17 (`crew` version 0.10.2.9003). Instead, the
  `seconds_interval` argument passed to
  [`crew_controller_group()`](https://wlandau.github.io/crew/reference/crew_controller_group.md)
  is used as `seconds_max` in a
  [`crew_throttle()`](https://wlandau.github.io/crew/reference/crew_throttle.md)
  object which orchestrates exponential backoff.

- `seconds_timeout`:

  Timeout length in seconds waiting for tasks.

- `scale`:

  Logical, whether to automatically call
  [`scale()`](https://rdrr.io/r/base/scale.html) to auto-scale workers
  to meet the demand of the task load. See also the `throttle` argument.

- `throttle`:

  `TRUE` to skip auto-scaling if it already happened within the last
  polling interval. `FALSE` to auto-scale every time
  [`scale()`](https://rdrr.io/r/base/scale.html) is called. Throttling
  avoids overburdening the `mirai` dispatcher and other resources.

- `controllers`:

  Not used. Included to ensure the signature is compatible with the
  analogous method of controller groups.

#### Details

The `wait()` method blocks the calling R session until the condition in
the `mode` argument is met. During the wait, `wait()` iteratively
auto-scales the workers.

#### Returns

A logical of length 1, invisibly. `wait(mode = "all")` returns `TRUE` if
all tasks in the `mirai` compute profile have resolved (`FALSE`
otherwise). `wait(mode = "one")` returns `TRUE` if the controller is
ready to pop or collect at least one resolved task (`FALSE` otherwise).
`wait(mode = "one")` assumes all tasks were submitted through the
controller and not by other means.

------------------------------------------------------------------------

### Method `push_backlog()`

Push the name of a task to the backlog.

#### Usage

    crew_class_controller$push_backlog(name, controller = NULL)

#### Arguments

- `name`:

  Character of length 1 with the task name to push to the backlog.

- `controller`:

  Not used. Included to ensure the signature is compatible with the
  analogous method of controller groups.

#### Details

`pop_backlog()` pops the tasks that can be pushed without saturating the
controller.

#### Returns

`NULL` (invisibly).

------------------------------------------------------------------------

### Method `pop_backlog()`

Pop the task names from the head of the backlog which can be pushed
without saturating the controller.

#### Usage

    crew_class_controller$pop_backlog(controllers = NULL)

#### Arguments

- `controllers`:

  Not used. Included to ensure the signature is compatible with the
  analogous method of controller groups.

#### Returns

Character vector of task names which can be pushed to the controller
without saturating it. If the controller is saturated, `character(0L)`
is returned.

------------------------------------------------------------------------

### Method [`summary()`](https://rdrr.io/r/base/summary.html)

Summarize the collected tasks of the controller.

#### Usage

    crew_class_controller$summary(controllers = NULL)

#### Arguments

- `controllers`:

  Not used. Included to ensure the signature is compatible with the
  analogous method of controller groups.

#### Returns

A data frame of cumulative summary statistics on the tasks collected
through `pop()` and `collect()`. It has one row and the following
columns:

- `controller`: name of the controller.

- `seconds`: total number of runtime in seconds.

- `tasks`: total number of tasks collected.

- `success`: total number of collected tasks that did not crash or
  error.

- `error`: total number of tasks with errors, either in the R code of
  the task or an NNG-level error that is not a cancellation or crash.

- `crash`: total number of crashed tasks (where the worker exited
  unexpectedly while it was running the task).

- `cancel`: total number of tasks interrupted with the `cancel()`
  controller method.

- `warning`: total number of tasks with one or more warnings.

------------------------------------------------------------------------

### Method `cancel()`

Cancel one or more tasks.

#### Usage

    crew_class_controller$cancel(names = character(0L), all = FALSE)

#### Arguments

- `names`:

  Character vector of names of tasks to cancel. Those names must have
  been manually supplied by `push()`.

- `all`:

  `TRUE` to cancel all tasks, `FALSE` otherwise. `all = TRUE` supersedes
  the `names` argument.

#### Returns

`NULL` (invisibly).

------------------------------------------------------------------------

### Method `pids()`

Deprecated on 2025-08-26 in `crew` version 1.2.1.9005.

#### Usage

    crew_class_controller$pids(controllers = NULL)

#### Arguments

- `controllers`:

  Not used. Included to ensure the signature is compatible with the
  analogous method of controller groups.

#### Returns

The integer process ID of the current process.

------------------------------------------------------------------------

### Method `terminate()`

Terminate the workers and the `mirai` client.

#### Usage

    crew_class_controller$terminate(controllers = NULL)

#### Arguments

- `controllers`:

  Not used. Included to ensure the signature is compatible with the
  analogous method of controller groups.

#### Returns

`NULL` (invisibly).

## Examples

``` r
if (identical(Sys.getenv("CREW_EXAMPLES"), "true")) {
client <- crew_client()
launcher <- crew_launcher_local()
controller <- crew_controller(client = client, launcher = launcher)
controller$start()
controller$push(name = "task", command = sqrt(4))
controller$wait()
controller$pop()
controller$terminate()
}

## ------------------------------------------------
## Method `crew_class_controller$new`
## ------------------------------------------------

if (identical(Sys.getenv("CREW_EXAMPLES"), "true")) {
client <- crew_client()
launcher <- crew_launcher_local()
controller <- crew_controller(client = client, launcher = launcher)
controller$start()
controller$push(name = "task", command = sqrt(4))
controller$wait()
controller$pop()
controller$terminate()
}
```
