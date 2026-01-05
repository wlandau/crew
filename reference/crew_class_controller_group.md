# Controller group class

`R6` class for controller groups.

## Details

See
[`crew_controller_group()`](https://wlandau.github.io/crew/reference/crew_controller_group.md).

## See also

Other controller_group:
[`crew_controller_group()`](https://wlandau.github.io/crew/reference/crew_controller_group.md)

## Active bindings

- `controllers`:

  List of `R6` controller objects.

- `relay`:

  Relay object for event-driven programming on a downstream condition
  variable.

## Methods

### Public methods

- [`crew_class_controller_group$new()`](#method-crew_class_controller_group-new)

- [`crew_class_controller_group$validate()`](#method-crew_class_controller_group-validate)

- [`crew_class_controller_group$size()`](#method-crew_class_controller_group-size)

- [`crew_class_controller_group$empty()`](#method-crew_class_controller_group-empty)

- [`crew_class_controller_group$nonempty()`](#method-crew_class_controller_group-nonempty)

- [`crew_class_controller_group$resolved()`](#method-crew_class_controller_group-resolved)

- [`crew_class_controller_group$unresolved()`](#method-crew_class_controller_group-unresolved)

- [`crew_class_controller_group$saturated()`](#method-crew_class_controller_group-saturated)

- [`crew_class_controller_group$start()`](#method-crew_class_controller_group-start)

- [`crew_class_controller_group$started()`](#method-crew_class_controller_group-started)

- [`crew_class_controller_group$launch()`](#method-crew_class_controller_group-launch)

- [`crew_class_controller_group$scale()`](#method-crew_class_controller_group-scale)

- [`crew_class_controller_group$autoscale()`](#method-crew_class_controller_group-autoscale)

- [`crew_class_controller_group$descale()`](#method-crew_class_controller_group-descale)

- [`crew_class_controller_group$crashes()`](#method-crew_class_controller_group-crashes)

- [`crew_class_controller_group$push()`](#method-crew_class_controller_group-push)

- [`crew_class_controller_group$walk()`](#method-crew_class_controller_group-walk)

- [`crew_class_controller_group$map()`](#method-crew_class_controller_group-map)

- [`crew_class_controller_group$pop()`](#method-crew_class_controller_group-pop)

- [`crew_class_controller_group$collect()`](#method-crew_class_controller_group-collect)

- [`crew_class_controller_group$wait()`](#method-crew_class_controller_group-wait)

- [`crew_class_controller_group$push_backlog()`](#method-crew_class_controller_group-push_backlog)

- [`crew_class_controller_group$pop_backlog()`](#method-crew_class_controller_group-pop_backlog)

- [`crew_class_controller_group$summary()`](#method-crew_class_controller_group-summary)

- [`crew_class_controller_group$pids()`](#method-crew_class_controller_group-pids)

- [`crew_class_controller_group$terminate()`](#method-crew_class_controller_group-terminate)

------------------------------------------------------------------------

### Method `new()`

Multi-controller constructor.

#### Usage

    crew_class_controller_group$new(controllers = NULL, relay = NULL)

#### Arguments

- `controllers`:

  List of `R6` controller objects.

- `relay`:

  Relay object for event-driven programming on a downstream condition
  variable.

#### Returns

An `R6` object with the controller group object.

#### Examples

    if (identical(Sys.getenv("CREW_EXAMPLES"), "true")) {
    persistent <- crew_controller_local(name = "persistent")
    transient <- crew_controller_local(
      name = "transient",
      tasks_max = 1L
    )
    group <- crew_controller_group(persistent, transient)
    group$start()
    group$push(name = "task", command = sqrt(4), controller = "transient")
    group$wait()
    group$pop()
    group$terminate()
    }

------------------------------------------------------------------------

### Method `validate()`

Validate the client.

#### Usage

    crew_class_controller_group$validate()

#### Returns

`NULL` (invisibly).

------------------------------------------------------------------------

### Method `size()`

Number of tasks in the selected controllers.

#### Usage

    crew_class_controller_group$size(controllers = NULL)

#### Arguments

- `controllers`:

  Character vector of controller names. Set to `NULL` to select all
  controllers.

#### Returns

Non-negative integer, number of tasks in the controller.

------------------------------------------------------------------------

### Method `empty()`

See if the controllers are empty.

#### Usage

    crew_class_controller_group$empty(controllers = NULL)

#### Arguments

- `controllers`:

  Character vector of controller names. Set to `NULL` to select all
  controllers.

#### Details

A controller is empty if it has no running tasks or completed tasks
waiting to be retrieved with `push()`.

#### Returns

`TRUE` if all the selected controllers are empty, `FALSE` otherwise.

------------------------------------------------------------------------

### Method `nonempty()`

Check if the controller group is nonempty.

#### Usage

    crew_class_controller_group$nonempty(controllers = NULL)

#### Arguments

- `controllers`:

  Character vector of controller names. Set to `NULL` to select all
  controllers.

#### Details

A controller is empty if it has no running tasks or completed tasks
waiting to be retrieved with `push()`.

#### Returns

`TRUE` if the controller is empty, `FALSE` otherwise.

------------------------------------------------------------------------

### Method `resolved()`

Number of resolved `mirai()` tasks.

#### Usage

    crew_class_controller_group$resolved(controllers = NULL)

#### Arguments

- `controllers`:

  Character vector of controller names. Set to `NULL` to select all
  controllers.

#### Details

`resolved()` is cumulative: it counts all the resolved tasks over the
entire lifetime of the controller session.

#### Returns

Non-negative integer of length 1, number of resolved `mirai()` tasks.
The return value is 0 if the condition variable does not exist (i.e. if
the client is not running).

------------------------------------------------------------------------

### Method `unresolved()`

Number of unresolved `mirai()` tasks.

#### Usage

    crew_class_controller_group$unresolved(controllers = NULL)

#### Arguments

- `controllers`:

  Character vector of controller names. Set to `NULL` to select all
  controllers.

#### Returns

Non-negative integer of length 1, number of unresolved `mirai()` tasks.

------------------------------------------------------------------------

### Method `saturated()`

Check if a controller is saturated.

#### Usage

    crew_class_controller_group$saturated(
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

  Character vector of length 1 with the controller name. Set to `NULL`
  to select the default controller that `push()` would choose.

#### Details

A controller is saturated if the number of uncollected tasks is greater
than or equal to the maximum number of workers. You can still push tasks
to a saturated controller, but tools that use `crew` such as `targets`
may choose not to (for performance and user-friendliness).

#### Returns

`TRUE` if all the selected controllers are saturated, `FALSE` otherwise.

------------------------------------------------------------------------

### Method [`start()`](https://rdrr.io/r/stats/start.html)

Start one or more controllers.

#### Usage

    crew_class_controller_group$start(controllers = NULL)

#### Arguments

- `controllers`:

  Character vector of controller names. Set to `NULL` to select all
  controllers.

#### Returns

`NULL` (invisibly).

------------------------------------------------------------------------

### Method `started()`

Check whether all the given controllers are started.

#### Usage

    crew_class_controller_group$started(controllers = NULL)

#### Arguments

- `controllers`:

  Character vector of controller names. Set to `NULL` to select all
  controllers.

#### Details

Actually checks whether all the given clients are started.

#### Returns

`TRUE` if the controllers are started, `FALSE` if any are not.

------------------------------------------------------------------------

### Method `launch()`

Launch one or more workers on one or more controllers.

#### Usage

    crew_class_controller_group$launch(n = 1L, controllers = NULL)

#### Arguments

- `n`:

  Number of workers to launch in each controller selected.

- `controllers`:

  Character vector of controller names. Set to `NULL` to select all
  controllers.

#### Returns

`NULL` (invisibly).

------------------------------------------------------------------------

### Method [`scale()`](https://rdrr.io/r/base/scale.html)

Automatically scale up the number of workers if needed in one or more
controller objects.

#### Usage

    crew_class_controller_group$scale(throttle = TRUE, controllers = NULL)

#### Arguments

- `throttle`:

  `TRUE` to skip auto-scaling if it already happened within the last
  polling interval. `FALSE` to auto-scale every time
  [`scale()`](https://rdrr.io/r/base/scale.html) is called. Throttling
  avoids overburdening the `mirai` dispatcher and other resources.

- `controllers`:

  Character vector of controller names. Set to `NULL` to select all
  controllers.

#### Details

See the [`scale()`](https://rdrr.io/r/base/scale.html) method in
individual controller classes.

#### Returns

Invisibly returns `TRUE` if there was any relevant auto-scaling activity
(new worker launches or worker connection/disconnection events) (`FALSE`
otherwise).

------------------------------------------------------------------------

### Method `autoscale()`

Run worker auto-scaling in a `later` loop.

#### Usage

    crew_class_controller_group$autoscale(
      loop = later::current_loop(),
      controllers = NULL
    )

#### Arguments

- `loop`:

  A `later` loop to run auto-scaling.

- `controllers`:

  Character vector of controller names. Set to `NULL` to select all
  controllers.

#### Returns

`NULL` (invisibly).

------------------------------------------------------------------------

### Method `descale()`

Terminate the auto-scaling loop started by `controller$autoscale()`.

#### Usage

    crew_class_controller_group$descale(controllers = NULL)

#### Arguments

- `controllers`:

  Character vector of controller names. Set to `NULL` to select all
  controllers.

#### Returns

`NULL` (invisibly).

------------------------------------------------------------------------

### Method `crashes()`

Report the number of consecutive crashes of a task, summed over all
selected controllers in the group.

#### Usage

    crew_class_controller_group$crashes(name, controllers = NULL)

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

Number of consecutive crashes of the named task, summed over all the
controllers in the group.

------------------------------------------------------------------------

### Method `push()`

Push a task to the head of the task list.

#### Usage

    crew_class_controller_group$push(
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
  for the task. See the `reset_globals` argument of
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

  Logical, whether to automatically scale workers to meet demand. See
  the `scale` argument of the `push()` method of ordinary single
  controllers.

- `throttle`:

  `TRUE` to skip auto-scaling if it already happened within the last
  polling interval. `FALSE` to auto-scale every time
  [`scale()`](https://rdrr.io/r/base/scale.html) is called. Throttling
  avoids overburdening the `mirai` dispatcher and other resources.

- `name`:

  Character string, name of the task. If `NULL`, a random name is
  automatically generated. The task name must not conflict with an
  existing task in the controller where it is submitted. To reuse the
  name, wait for the existing task to finish, then either `pop()` or
  `collect()` it to remove it from its controller.

- `save_command`:

  Deprecated on 2025-01-22 (`crew` version 0.10.2.9004).

- `controller`:

  Character of length 1, name of the controller to submit the task. If
  `NULL`, the controller defaults to the first controller in the list.

#### Returns

Invisibly return the `mirai` object of the pushed task. This allows you
to interact with the task directly, e.g. to create a promise object with
[`promises::as.promise()`](https://rstudio.github.io/promises/reference/is.promise.html).

------------------------------------------------------------------------

### Method `walk()`

Apply a single command to multiple inputs, and return control to the
user without waiting for any task to complete.

#### Usage

    crew_class_controller_group$walk(
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

  Deprecated on 2025-01-22 (`crew` version 0.10.2.9004).

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

  Character of length 1, name of the controller to submit the tasks. If
  `NULL`, the controller defaults to the first controller in the list.

#### Details

In contrast to `walk()`, `map()` blocks the local R session and waits
for all tasks to complete.

#### Returns

Invisibly returns a list of `mirai` task objects for the newly created
tasks. The order of tasks in the list matches the order of data in the
`iterate` argument.

------------------------------------------------------------------------

### Method `map()`

Apply a single command to multiple inputs.

#### Usage

    crew_class_controller_group$map(
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

  Optional character of length 1, name of the element of `iterate` with
  names for the tasks. If `names` is supplied, then `iterate[[names]]`
  must be a character vector.

- `save_command`:

  Deprecated on 2025-01-22 (`crew` version 0.10.2.9004).

- `error`:

  Character vector of length 1, choice of action if a task has an error.
  Possible values:

  - `"stop"`: throw an error in the main R session instead of returning
    a value. In case of an error, the results from the last errored
    `map()` are in the `error` field of the controller, e.g.
    `controller_object$error`. To reduce memory consumption, set
    `controller_object$error <- NULL` after you are finished
    troubleshooting.

  - `"warn"`: throw a warning. This allows the return value with all the
    error messages and tracebacks to be generated.

  - `"silent"`: do nothing special.

- `warnings`:

  Logical of length 1, whether to throw a warning in the interactive
  session if at least one task encounters an error.

- `verbose`:

  Logical of length 1, whether to print progress messages.

- `scale`:

  Logical, whether to automatically scale workers to meet demand. See
  also the `throttle` argument.

- `throttle`:

  `TRUE` to skip auto-scaling if it already happened within the last
  polling interval. `FALSE` to auto-scale every time
  [`scale()`](https://rdrr.io/r/base/scale.html) is called. Throttling
  avoids overburdening the `mirai` dispatcher and other resources.

- `controller`:

  Character of length 1, name of the controller to submit the tasks. If
  `NULL`, the controller defaults to the first controller in the list.

#### Details

The idea comes from functional programming: for example, the `map()`
function from the `purrr` package.

#### Returns

A `tibble` of results and metadata: one row per task and columns
corresponding to the output of `pop()`.

------------------------------------------------------------------------

### Method `pop()`

Pop a completed task from the results data frame.

#### Usage

    crew_class_controller_group$pop(
      scale = TRUE,
      collect = NULL,
      throttle = TRUE,
      error = NULL,
      controllers = NULL
    )

#### Arguments

- `scale`:

  Logical, whether to automatically scale workers to meet demand. See
  the `scale` argument of the `pop()` method of ordinary single
  controllers.

- `collect`:

  Deprecated in version 0.5.0.9003 (2023-10-02). Not used.

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

  - `NULL` or `"silent"`: do not react to errors.

- `controllers`:

  Character vector of controller names. Set to `NULL` to select all
  controllers.

#### Returns

If there is no task to collect, return `NULL`. Otherwise, return a
one-row `tibble` with the same columns as `pop()` for ordinary
controllers.

------------------------------------------------------------------------

### Method `collect()`

Pop all available task results and return them in a tidy `tibble`.

#### Usage

    crew_class_controller_group$collect(
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
  threw an error. Possible values:

  - `"stop"`: throw an error in the main R session instead of returning
    a value.

  - `"warn"`: throw a warning.

  - `NULL` or `"silent"`: do not react to errors.

- `controllers`:

  Character vector of controller names. Set to `NULL` to select all
  controllers.

#### Returns

A `tibble` of results and metadata of all resolved tasks, with one row
per task. Returns `NULL` if there are no available results.

------------------------------------------------------------------------

### Method `wait()`

Wait for tasks.

#### Usage

    crew_class_controller_group$wait(
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

  Timeout length in seconds waiting for results to become available.

- `scale`:

  Logical of length 1, whether to call `scale_later()` on each selected
  controller to schedule auto-scaling. See the `scale` argument of the
  `wait()` method of ordinary single controllers.

- `throttle`:

  `TRUE` to skip auto-scaling if it already happened within the last
  polling interval. `FALSE` to auto-scale every time
  [`scale()`](https://rdrr.io/r/base/scale.html) is called. Throttling
  avoids overburdening the `mirai` dispatcher and other resources.

- `controllers`:

  Character vector of controller names. Set to `NULL` to select all
  controllers.

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

    crew_class_controller_group$push_backlog(name, controller = NULL)

#### Arguments

- `name`:

  Character of length 1 with the task name to push to the backlog.

- `controller`:

  Character vector of length 1 with the controller name. Set to `NULL`
  to select the default controller that `push_backlog()` would choose.

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

    crew_class_controller_group$pop_backlog(controllers = NULL)

#### Arguments

- `controllers`:

  Character vector of controller names. Set to `NULL` to select all
  controllers.

#### Returns

Character vector of task names which can be pushed to the controller
without saturating it. If the controller is saturated, `character(0L)`
is returned.

------------------------------------------------------------------------

### Method [`summary()`](https://rdrr.io/r/base/summary.html)

Summarize the workers of one or more controllers.

#### Usage

    crew_class_controller_group$summary(controllers = NULL)

#### Arguments

- `controllers`:

  Character vector of controller names. Set to `NULL` to select all
  controllers.

#### Returns

A data frame of aggregated worker summary statistics of all the selected
controllers. It has one row per worker, and the rows are grouped by
controller. See the documentation of the
[`summary()`](https://rdrr.io/r/base/summary.html) method of the
controller class for specific information about the columns in the
output.

------------------------------------------------------------------------

### Method `pids()`

Deprecated on 2025-08-26 in `crew` version 1.2.1.9005.

#### Usage

    crew_class_controller_group$pids(controllers = NULL)

#### Arguments

- `controllers`:

  Character vector of controller names. Set to `NULL` to select all
  controllers.

#### Returns

The integer process ID of the current process.

------------------------------------------------------------------------

### Method `terminate()`

Terminate the workers and disconnect the client for one or more
controllers.

#### Usage

    crew_class_controller_group$terminate(controllers = NULL)

#### Arguments

- `controllers`:

  Character vector of controller names. Set to `NULL` to select all
  controllers.

#### Returns

`NULL` (invisibly).

## Examples

``` r
if (identical(Sys.getenv("CREW_EXAMPLES"), "true")) {
persistent <- crew_controller_local(name = "persistent")
transient <- crew_controller_local(
  name = "transient",
  tasks_max = 1L
)
group <- crew_controller_group(persistent, transient)
group$start()
group$push(name = "task", command = sqrt(4), controller = "transient")
group$wait()
group$pop()
group$terminate()
}

## ------------------------------------------------
## Method `crew_class_controller_group$new`
## ------------------------------------------------

if (identical(Sys.getenv("CREW_EXAMPLES"), "true")) {
persistent <- crew_controller_local(name = "persistent")
transient <- crew_controller_local(
  name = "transient",
  tasks_max = 1L
)
group <- crew_controller_group(persistent, transient)
group$start()
group$push(name = "task", command = sqrt(4), controller = "transient")
group$wait()
group$pop()
group$terminate()
}
```
