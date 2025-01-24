#' @title Create a controller group.
#' @export
#' @family controller_group
#' @description Create an `R6` object to submit tasks and launch workers
#'   through multiple `crew` controllers.
#' @inheritParams crew_client
#' @param ... `R6` controller objects or lists of `R6` controller objects.
#'   Nested lists are allowed, but each element must be a control object
#'   or another list.
#' @examples
#' if (identical(Sys.getenv("CREW_EXAMPLES"), "true")) {
#' persistent <- crew_controller_local(name = "persistent")
#' transient <- crew_controller_local(
#'   name = "transient",
#'   tasks_max = 1L
#' )
#' group <- crew_controller_group(persistent, transient)
#' group$start()
#' group$push(name = "task", command = sqrt(4), controller = "transient")
#' group$wait()
#' group$pop()
#' group$terminate()
#' }
crew_controller_group <- function(..., seconds_interval = 1) {
  controllers <- unlist(list(...), recursive = TRUE)
  crew_assert(
    length(controllers) > 0L,
    message = "A controller group must have one or more crew controllers."
  )
  crew_assert(
    map_lgl(controllers, inherits, what = "crew_class_controller"),
    message = "Found invalid controllers while creating a controller group."
  )
  names(controllers) <- map_chr(controllers, ~.x$launcher$name)
  walk(
    names(controllers),
    ~crew_assert(
      !isTRUE(controllers[[.x]]$client$started),
      message = paste(
        "controller",
        .x,
        "supplied to crew_controller_group()",
        "must not already be started."
      )
    )
  )
  relay <- crew_relay()
  relay$start()
  throttle <- crew_throttle(seconds_max = seconds_interval)
  for (controller in controllers) {
    controller$client$relay$set_to(relay$condition)
  }
  out <- crew_class_controller_group$new(
    controllers = controllers,
    relay = relay,
    throttle = throttle
  )
  out$validate()
  out
}

#' @title Controller group class
#' @export
#' @family controller_group
#' @description `R6` class for controller groups.
#' @details See [crew_controller_group()].
#' @examples
#' if (identical(Sys.getenv("CREW_EXAMPLES"), "true")) {
#' persistent <- crew_controller_local(name = "persistent")
#' transient <- crew_controller_local(
#'   name = "transient",
#'   tasks_max = 1L
#' )
#' group <- crew_controller_group(persistent, transient)
#' group$start()
#' group$push(name = "task", command = sqrt(4), controller = "transient")
#' group$wait()
#' group$pop()
#' group$terminate()
#' }
crew_class_controller_group <- R6::R6Class(
  classname = "crew_class_controller_group",
  cloneable = FALSE,
  private = list(
    .controllers = NULL,
    .relay = NULL,
    .throttle = NULL,
    .select_controllers = function(names) {
      if (is.null(names)) {
        return(private$.controllers)
      }
      message <- "'controllers' must be a valid nonempty character vector."
      crew_assert(
        names,
        length(.) > 0L,
        is.character(.),
        !anyNA(.),
        nzchar(.),
        message = message
      )
      invalid <- setdiff(names, names(private$.controllers))
      crew_assert(
        !length(invalid),
        message = sprintf(
          "controllers not found: %s",
          paste(invalid, collapse = ", ")
        )
      )
      private$.controllers[names]
    },
    .select_single_controller = function(name) {
      names <- names(private$.controllers)
      name <- name %|||% utils::head(names, n = 1L)
      crew_assert(
        name %in% names,
        message = sprintf("controller not found: %s", name)
      )
      private$.controllers[[name]]
    },
    .wait_one = function(
      controllers,
      control,
      seconds_timeout,
      scale,
      throttle
    ) {
      if (sum(map_int(control, ~length(.x$tasks))) < 1L) {
        return(FALSE)
      }
      envir <- new.env(parent = emptyenv())
      envir$result <- FALSE
      iterate <- function() {
        if (scale) {
          self$scale(throttle = throttle, controllers = controllers)
        }
        for (controller in control) {
          if (controller$unpopped() > 0L) {
            envir$result <- TRUE
            return(TRUE)
          }
        }
        private$.relay$wait(throttle = private$.throttle)
        FALSE
      }
      crew_retry(
        fun = iterate,
        seconds_interval = 0,
        seconds_timeout = seconds_timeout,
        error = FALSE,
        assertions = FALSE
      )
      envir$result
    },
    .wait_all = function(
      controllers,
      control,
      seconds_timeout,
      scale,
      throttle
    ) {
      if (sum(map_int(control, ~length(.x$tasks))) < 1L) {
        return(TRUE)
      }
      envir <- new.env(parent = emptyenv())
      envir$result <- FALSE
      iterate <- function() {
        if (scale) {
          self$scale(throttle = throttle, controllers = controllers)
        }
        envir$result <- sum(map_int(control, ~.x$unresolved())) < 1L
        if (!envir$result) {
          private$.relay$wait(throttle = private$.throttle)
        }
        envir$result
      }
      crew_retry(
        fun = iterate,
        seconds_interval = 0,
        seconds_timeout = seconds_timeout,
        error = FALSE,
        assertions = FALSE
      )
      envir$result
    }
  ),
  active = list(
    #' @field controllers List of `R6` controller objects.
    controllers = function() {
      .subset2(private, ".controllers")
    },
    #' @field relay Relay object for event-driven programming on a downstream
    #'   condition variable.
    relay = function() {
      .subset2(private, ".relay")
    },
    #' @field throttle [crew_throttle()] object to orchestrate exponential
    #'  backoff in the relay and auto-scaling.
    throttle = function() {
      .subset2(private, ".throttle")
    }
  ),
  public = list(
    #' @description Multi-controller constructor.
    #' @return An `R6` object with the controller group object.
    #' @param controllers List of `R6` controller objects.
    #' @param relay Relay object for event-driven programming on a downstream
    #'   condition variable.
    #' @param throttle [crew_throttle()] object to orchestrate exponential
    #'  backoff in the relay and auto-scaling.
    #' @examples
    #' if (identical(Sys.getenv("CREW_EXAMPLES"), "true")) {
    #' persistent <- crew_controller_local(name = "persistent")
    #' transient <- crew_controller_local(
    #'   name = "transient",
    #'   tasks_max = 1L
    #' )
    #' group <- crew_controller_group(persistent, transient)
    #' group$start()
    #' group$push(name = "task", command = sqrt(4), controller = "transient")
    #' group$wait()
    #' group$pop()
    #' group$terminate()
    #' }
    initialize = function(
      controllers = NULL,
      relay = NULL,
      throttle = NULL
    ) {
      private$.controllers <- controllers
      private$.relay <- relay
      private$.throttle <- throttle
      invisible()
    },
    #' @description Validate the client.
    #' @return `NULL` (invisibly).
    validate = function() {
      crew_assert(
        map_lgl(private$.controllers, ~inherits(.x, "crew_class_controller")),
        message = "All objects in a controller group must be controllers."
      )
      out <- unname(map_chr(private$.controllers, ~.x$launcher$name))
      exp <- names(private$.controllers)
      crew_assert(identical(out, exp), message = "bad controller names")
      crew_assert(inherits(private$.relay, "crew_class_relay"))
      private$.relay$validate()
      private$.throttle$validate()
      invisible()
    },
    #' @description See if the controllers are empty.
    #' @details A controller is empty if it has no running tasks
    #'   or completed tasks waiting to be retrieved with `push()`.
    #' @return `TRUE` if all the selected controllers are empty,
    #'   `FALSE` otherwise.
    #' @param controllers Character vector of controller names.
    #'   Set to `NULL` to select all controllers.
    empty = function(controllers = NULL) {
      control <- private$.select_controllers(controllers)
      all(map_lgl(control, ~.x$empty()))
    },
    #' @description Check if the controller group is nonempty.
    #' @details A controller is empty if it has no running tasks
    #'   or completed tasks waiting to be retrieved with `push()`.
    #' @return `TRUE` if the controller is empty, `FALSE` otherwise.
    #' @param controllers Character vector of controller names.
    #'   Set to `NULL` to select all controllers.
    nonempty = function(controllers = NULL) {
      control <- private$.select_controllers(controllers)
      any(map_lgl(control, ~.x$nonempty()))
    },
    #' @description Number of resolved `mirai()` tasks.
    #' @details `resolved()` is cumulative: it counts all the resolved
    #'   tasks over the entire lifetime of the controller session.
    #' @return Non-negative integer of length 1,
    #'   number of resolved `mirai()` tasks.
    #'   The return value is 0 if the condition variable does not exist
    #'   (i.e. if the client is not running).
    #' @param controllers Character vector of controller names.
    #'   Set to `NULL` to select all controllers.
    resolved = function(controllers = NULL) {
      control <- private$.select_controllers(controllers)
      sum(map_int(control, ~.x$resolved()))
    },
    #' @description Number of unresolved `mirai()` tasks.
    #' @return Non-negative integer of length 1,
    #'   number of unresolved `mirai()` tasks.
    #' @param controllers Character vector of controller names.
    #'   Set to `NULL` to select all controllers.
    unresolved = function(controllers = NULL) {
      control <- private$.select_controllers(controllers)
      sum(map_int(control, ~.x$unresolved()))
    },
    #' @description Number of resolved `mirai()` tasks available via `pop()`.
    #' @return Non-negative integer of length 1,
    #'   number of resolved `mirai()` tasks available via `pop()`.
    #' @param controllers Character vector of controller names.
    #'   Set to `NULL` to select all controllers.
    unpopped = function(controllers = NULL) {
      control <- private$.select_controllers(controllers)
      sum(map_int(control, ~.x$unpopped()))
    },
    #' @description Check if a controller is saturated.
    #' @details A controller is saturated if the number of unresolved tasks
    #'   is greater than or equal to the maximum number of workers.
    #'   In other words, in a saturated controller, every available worker
    #'   has a task.
    #'   You can still push tasks to a saturated controller, but
    #'   tools that use `crew` such as `targets` may choose not to.
    #' @return `TRUE` if all the selected controllers are saturated,
    #'   `FALSE` otherwise.
    #' @param collect Deprecated in version 0.5.0.9003 (2023-10-02). Not used.
    #' @param throttle Deprecated in version 0.5.0.9003 (2023-10-02). Not used.
    #' @param controller Character vector of length 1 with the controller name.
    #'   Set to `NULL` to select the default controller that `push()`
    #'   would choose.
    saturated = function(collect = NULL, throttle = NULL, controller = NULL) {
      control <- private$.select_single_controller(name = controller)
      control$saturated()
    },
    #' @description Start one or more controllers.
    #' @return `NULL` (invisibly).
    #' @param controllers Character vector of controller names.
    #'   Set to `NULL` to select all controllers.
    start = function(controllers = NULL) {
      control <- private$.select_controllers(controllers)
      walk(control, ~.x$start())
    },
    #' @description Check whether all the given controllers are started.
    #' @details Actually checks whether all the given clients are started.
    #' @return `TRUE` if the controllers are started, `FALSE` if any are not.
    #' @param controllers Character vector of controller names.
    #'   Set to `NULL` to select all controllers.
    started = function(controllers = NULL) {
      control <- private$.select_controllers(controllers)
      all(map_lgl(control, ~.x$started()))
    },
    #' @description Launch one or more workers on one or more controllers.
    #' @return `NULL` (invisibly).
    #' @param n Number of workers to launch in each controller selected.
    #' @param controllers Character vector of controller names.
    #'   Set to `NULL` to select all controllers.
    launch = function(n = 1L, controllers = NULL) {
      control <- private$.select_controllers(controllers)
      walk(control, ~.x$launch(n = n))
    },
    #' @description Automatically scale up the number of workers if needed
    #'   in one or more controller objects.
    #' @details See the `scale()` method in individual controller classes.
    #' @return Invisibly returns `TRUE` if there was any relevant
    #'   auto-scaling activity (new worker launches or worker
    #'   connection/disconnection events) (`FALSE` otherwise).
    #' @param throttle `TRUE` to skip auto-scaling if it already happened
    #'   within the last `seconds_interval` seconds. `FALSE` to auto-scale
    #'   every time `scale()` is called. Throttling avoids
    #'   overburdening the `mirai` dispatcher and other resources.
    #' @param controllers Character vector of controller names.
    #'   Set to `NULL` to select all controllers.
    scale = function(throttle = TRUE, controllers = NULL) {
      control <- private$.select_controllers(controllers)
      if (throttle && !private$.throttle$poll()) {
        return(invisible())
      }
      activity <- any(map_lgl(control, ~.x$scale(throttle = FALSE)))
      private$.throttle$update(activity = activity)
      invisible(activity)
    },
    #' @description Run worker auto-scaling in a private `later` loop
    #'   every `controller$client$seconds_interval` seconds.
    #' @param controllers Character vector of controller names.
    #'   Set to `NULL` to select all controllers.
    #' @return `NULL` (invisibly).
    autoscale = function(controllers = NULL) {
      # Tested in tests/interactive/test-promises.R
      # nocov start
      control <- private$.select_controllers(controllers)
      walk(control, ~.x$autoscale())
      # nocov end
    },
    #' @description Terminate the auto-scaling loop started by
    #'   `controller$autoscale()`.
    #' @param controllers Character vector of controller names.
    #'   Set to `NULL` to select all controllers.
    #' @return `NULL` (invisibly).
    descale = function(controllers = NULL) {
      control <- private$.select_controllers(controllers)
      walk(control, ~.x$descale())
    },
    #' @description Report the number of consecutive crashes of a task,
    #'   summed over all selected controllers in the group.
    #' @details See the `crashes_max` argument of [crew_controller()].
    #' @return Number of consecutive crashes of the named task,
    #'   summed over all the controllers in the group.
    #' @param name Character string, name of the task to check.
    #' @param controllers Not used. Included to ensure the signature is
    #'   compatible with the analogous method of controller groups.
    crashes = function(name, controllers = NULL) {
      control <- private$.select_controllers(controllers)
      sum(map_int(control, ~.x$crashes(name = name)))
    },
    #' @description Push a task to the head of the task list.
    #' @return Invisibly return the `mirai` object of the pushed task.
    #'   This allows you to interact with the task directly, e.g.
    #'   to create a promise object with `promises::as.promise()`.
    #' @param command Language object with R code to run.
    #' @param data Named list of local data objects in the
    #'   evaluation environment.
    #' @param globals Named list of objects to temporarily assign to the
    #'   global environment for the task. See the `reset_globals`
    #'   argument of [crew_controller_local()].
    #' @param substitute Logical of length 1, whether to call
    #'   `base::substitute()` on the supplied value of the
    #'   `command` argument. If `TRUE` (default) then `command` is quoted
    #'   literally as you write it, e.g.
    #'   `push(command = your_function_call())`. If `FALSE`, then `crew`
    #'   assumes `command` is a language object and you are passing its
    #'   value, e.g. `push(command = quote(your_function_call()))`.
    #'   `substitute = TRUE` is appropriate for interactive use,
    #'   whereas `substitute = FALSE` is meant for automated R programs
    #'   that invoke `crew` controllers.
    #' @param seed Integer of length 1 with the pseudo-random number generator
    #'   seed to set for the evaluation of the task. Passed to the
    #'   `seed` argument of `set.seed()` if not `NULL`.
    #'   If `algorithm` and `seed` are both `NULL`,
    #'   then the random number generator defaults to the
    #'   widely spaced worker-specific
    #'   L'Ecuyer streams as supported by `mirai::nextstream()`.
    #'   See `vignette("parallel", package = "parallel")` for details.
    #' @param algorithm Integer of length 1 with the pseudo-random number
    #'   generator algorithm to set for the evaluation of the task.
    #'   Passed to the `kind` argument of `RNGkind()` if not `NULL`.
    #'   If `algorithm` and `seed` are both `NULL`,
    #'   then the random number generator defaults to the
    #'   recommended widely spaced worker-specific
    #'   L'Ecuyer streams as supported by `mirai::nextstream()`.
    #'   See `vignette("parallel", package = "parallel")` for details.
    #' @param packages Character vector of packages to load for the task.
    #' @param library Library path to load the packages. See the `lib.loc`
    #'   argument of `require()`.
    #' @param seconds_timeout Optional task timeout passed to the `.timeout`
    #'   argument of `mirai::mirai()` (after converting to milliseconds).
    #' @param scale Logical, whether to automatically scale workers to meet
    #'   demand. See the `scale` argument of the `push()` method of
    #'   ordinary single controllers.
    #' @param throttle `TRUE` to skip auto-scaling if it already happened
    #'   within the last `seconds_interval` seconds. `FALSE` to auto-scale
    #'   every time `scale()` is called. Throttling avoids
    #'   overburdening the `mirai` dispatcher and other resources.
    #' @param name Character string, name of the task. If `NULL`,
    #'   a random name is automatically generated.
    #'   The task name must not conflict with an existing task
    #'   in the controller where it is submitted.
    #'   To reuse the name, wait for the existing task
    #'   to finish, then either `pop()` or `collect()` it
    #'   to remove it from its controller.
    #' @param save_command Deprecated on 2025-01-22
    #'   (`crew` version 0.10.2.9004).
    #' @param controller Character of length 1,
    #'   name of the controller to submit the task.
    #'   If `NULL`, the controller defaults to the
    #'   first controller in the list.
    push = function(
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
    ) {
      if (substitute) {
        command <- substitute(command)
      }
      control <- .subset2(
        private,
        ".select_single_controller"
      )(name = controller)
      .subset2(control, "push")(
        command = command,
        data = data,
        globals = globals,
        substitute = FALSE,
        seed = seed,
        algorithm = algorithm,
        packages = packages,
        library = library,
        seconds_timeout = seconds_timeout,
        scale = scale,
        throttle = throttle,
        name = name
      )
    },
    #' @description Apply a single command to multiple inputs,
    #'   and return control to the user without
    #'   waiting for any task to complete.
    #' @details In contrast to `walk()`, `map()` blocks the local R session
    #'   and waits for all tasks to complete.
    #' @return Invisibly returns a list of `mirai` task objects for the
    #'   newly created tasks. The order of tasks in the list matches the
    #'   order of data in the `iterate` argument.
    #' @param command Language object with R code to run.
    #' @param iterate Named list of vectors or lists to iterate over.
    #'   For example, to run function calls
    #'   `f(x = 1, y = "a")` and `f(x = 2, y = "b")`,
    #'   set `command` to `f(x, y)`, and set `iterate` to
    #'   `list(x = c(1, 2), y = c("a", "b"))`. The individual
    #'   function calls are evaluated as
    #'   `f(x = iterate$x[[1]], y = iterate$y[[1]])` and
    #'   `f(x = iterate$x[[2]], y = iterate$y[[2]])`.
    #'   All the elements of `iterate` must have the same length.
    #'   If there are any name conflicts between `iterate` and `data`,
    #'   `iterate` takes precedence.
    #' @param data Named list of constant local data objects in the
    #'   evaluation environment. Objects in this list are treated as single
    #'   values and are held constant for each iteration of the map.
    #' @param globals Named list of constant objects to temporarily
    #'   assign to the global environment for each task. This list should
    #'   include any functions you previously defined in the global
    #'   environment which are required to run tasks.
    #'   See the `reset_globals` argument of [crew_controller_local()].
    #'   Objects in this list are treated as single
    #'   values and are held constant for each iteration of the map.
    #' @param substitute Logical of length 1, whether to call
    #'   `base::substitute()` on the supplied value of the
    #'   `command` argument. If `TRUE` (default) then `command` is quoted
    #'   literally as you write it, e.g.
    #'   `push(command = your_function_call())`. If `FALSE`, then `crew`
    #'   assumes `command` is a language object and you are passing its
    #'   value, e.g. `push(command = quote(your_function_call()))`.
    #'   `substitute = TRUE` is appropriate for interactive use,
    #'   whereas `substitute = FALSE` is meant for automated R programs
    #'   that invoke `crew` controllers.
    #' @param seed Integer of length 1 with the pseudo-random number generator
    #'   seed to set for the evaluation of the task. Passed to the
    #'   `seed` argument of `set.seed()` if not `NULL`.
    #'   If `algorithm` and `seed` are both `NULL`,
    #'   then the random number generator defaults to the
    #'   recommended widely spaced worker-specific
    #'   L'Ecuyer streams as supported by `mirai::nextstream()`.
    #'   See `vignette("parallel", package = "parallel")` for details.
    #' @param algorithm Integer of length 1 with the pseudo-random number
    #'   generator algorithm to set for the evaluation of the task.
    #'   Passed to the `kind` argument of `RNGkind()` if not `NULL`.
    #'   If `algorithm` and `seed` are both `NULL`,
    #'   then the random number generator defaults to the
    #'   recommended widely spaced worker-specific
    #'   L'Ecuyer streams as supported by `mirai::nextstream()`.
    #'   See `vignette("parallel", package = "parallel")` for details.
    #' @param packages Character vector of packages to load for the task.
    #' @param library Library path to load the packages. See the `lib.loc`
    #'   argument of `require()`.
    #' @param seconds_timeout Optional task timeout passed to the `.timeout`
    #'   argument of `mirai::mirai()` (after converting to milliseconds).
    #' @param names Optional character of length 1, name of the element of
    #'   `iterate` with names for the tasks. If `names` is supplied,
    #'   then `iterate[[names]]` must be a character vector.
    #' @param save_command Deprecated on 2025-01-22
    #'   (`crew` version 0.10.2.9004).
    #' @param scale Logical, whether to automatically scale workers to meet
    #'   demand. See also the `throttle` argument.
    #' @param throttle `TRUE` to skip auto-scaling if it already happened
    #'   within the last `seconds_interval` seconds. `FALSE` to auto-scale
    #'   every time `scale()` is called. Throttling avoids
    #'   overburdening the `mirai` dispatcher and other resources.
    #' @param controller Character of length 1,
    #'   name of the controller to submit the tasks.
    #'   If `NULL`, the controller defaults to the
    #'   first controller in the list.
    walk = function(
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
      scale = TRUE,
      throttle = TRUE,
      controller = NULL
    ) {
      crew_assert(substitute, isTRUE(.) || isFALSE(.))
      if (substitute) {
        command <- substitute(command)
      }
      control <- private$.select_single_controller(name = controller)
      control$walk(
        command = command,
        iterate = iterate,
        data = data,
        globals = globals,
        substitute = FALSE,
        seed = seed,
        algorithm = algorithm,
        packages = packages,
        library = library,
        seconds_timeout = seconds_timeout,
        names = names,
        scale = scale,
        throttle = throttle
      )
    },
    #' @description Apply a single command to multiple inputs.
    #' @details The idea comes from functional programming: for example,
    #'   the `map()` function from the `purrr` package.
    #' @return A `tibble` of results and metadata: one row per task and
    #'   columns corresponding to the output of `pop()`.
    #' @param command Language object with R code to run.
    #' @param iterate Named list of vectors or lists to iterate over.
    #'   For example, to run function calls
    #'   `f(x = 1, y = "a")` and `f(x = 2, y = "b")`,
    #'   set `command` to `f(x, y)`, and set `iterate` to
    #'   `list(x = c(1, 2), y = c("a", "b"))`. The individual
    #'   function calls are evaluated as
    #'   `f(x = iterate$x[[1]], y = iterate$y[[1]])` and
    #'   `f(x = iterate$x[[2]], y = iterate$y[[2]])`.
    #'   All the elements of `iterate` must have the same length.
    #'   If there are any name conflicts between `iterate` and `data`,
    #'   `iterate` takes precedence.
    #' @param data Named list of constant local data objects in the
    #'   evaluation environment. Objects in this list are treated as single
    #'   values and are held constant for each iteration of the map.
    #' @param globals Named list of constant objects to temporarily
    #'   assign to the global environment for each task. This list should
    #'   include any functions you previously defined in the global
    #'   environment which are required to run tasks.
    #'   See the `reset_globals` argument of [crew_controller_local()].
    #'   Objects in this list are treated as single
    #'   values and are held constant for each iteration of the map.
    #' @param substitute Logical of length 1, whether to call
    #'   `base::substitute()` on the supplied value of the
    #'   `command` argument. If `TRUE` (default) then `command` is quoted
    #'   literally as you write it, e.g.
    #'   `push(command = your_function_call())`. If `FALSE`, then `crew`
    #'   assumes `command` is a language object and you are passing its
    #'   value, e.g. `push(command = quote(your_function_call()))`.
    #'   `substitute = TRUE` is appropriate for interactive use,
    #'   whereas `substitute = FALSE` is meant for automated R programs
    #'   that invoke `crew` controllers.
    #' @param seed Integer of length 1 with the pseudo-random number generator
    #'   seed to set for the evaluation of the task. Passed to the
    #'   `seed` argument of `set.seed()` if not `NULL`.
    #'   If `algorithm` and `seed` are both `NULL`,
    #'   then the random number generator defaults to the
    #'   recommended widely spaced worker-specific
    #'   L'Ecuyer streams as supported by `mirai::nextstream()`.
    #'   See `vignette("parallel", package = "parallel")` for details.
    #' @param algorithm Integer of length 1 with the pseudo-random number
    #'   generator algorithm to set for the evaluation of the task.
    #'   Passed to the `kind` argument of `RNGkind()` if not `NULL`.
    #'   If `algorithm` and `seed` are both `NULL`,
    #'   then the random number generator defaults to the
    #'   recommended widely spaced worker-specific
    #'   L'Ecuyer streams as supported by `mirai::nextstream()`.
    #'   See `vignette("parallel", package = "parallel")` for details.
    #' @param packages Character vector of packages to load for the task.
    #' @param library Library path to load the packages. See the `lib.loc`
    #'   argument of `require()`.
    #' @param seconds_interval Deprecated on 2025-01-17 (`crew` version
    #'   0.10.2.9003). Instead, the `seconds_interval` argument passed
    #'   to [crew_controller_group()] is used as `seconds_max`
    #'   in a [crew_throttle()] object which orchestrates exponential
    #'   backoff.
    #' @param seconds_timeout Optional task timeout passed to the `.timeout`
    #'   argument of `mirai::mirai()` (after converting to milliseconds).
    #' @param names Optional character of length 1, name of the element of
    #'   `iterate` with names for the tasks. If `names` is supplied,
    #'   then `iterate[[names]]` must be a character vector.
    #' @param save_command Deprecated on 2025-01-22
    #'   (`crew` version 0.10.2.9004).
    #' @param error Character vector of length 1, choice of action if
    #'   a task has an error. Possible values:
    #'   * `"stop"`: throw an error in the main R session instead of returning
    #'     a value. In case of an error, the results from the last errored
    #'     `map()` are in the `error` field
    #'     of the controller, e.g. `controller_object$error`. To reduce
    #'     memory consumption, set `controller_object$error <- NULL` after
    #'     you are finished troubleshooting.
    #'   * `"warn"`: throw a warning. This allows the return value with
    #'     all the error messages and tracebacks to be generated.
    #'   * `"silent"`: do nothing special.
    #' @param warnings Logical of length 1, whether to throw a warning in the
    #'   interactive session if at least one task encounters an error.
    #' @param verbose Logical of length 1, whether to print progress messages.
    #' @param scale Logical, whether to automatically scale workers to meet
    #'   demand. See also the `throttle` argument.
    #' @param throttle `TRUE` to skip auto-scaling if it already happened
    #'   within the last `seconds_interval` seconds. `FALSE` to auto-scale
    #'   every time `scale()` is called. Throttling avoids
    #'   overburdening the `mirai` dispatcher and other resources.
    #' @param controller Character of length 1,
    #'   name of the controller to submit the tasks.
    #'   If `NULL`, the controller defaults to the
    #'   first controller in the list.
    map = function(
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
    ) {
      crew_deprecate(
        name = "save_command",
        date = "2025-01-22",
        version = "0.10.2.9004",
        alternative = "none (no longer used)",
        condition = "warning",
        value = save_command
      )
      crew_assert(substitute, isTRUE(.) || isFALSE(.))
      if (substitute) {
        command <- substitute(command)
      }
      control <- private$.select_single_controller(name = controller)
      control$map(
        command = command,
        iterate = iterate,
        data = data,
        globals = globals,
        substitute = FALSE,
        seed = seed,
        algorithm = algorithm,
        packages = packages,
        library = library,
        seconds_timeout = seconds_timeout,
        names = names,
        error = error,
        warnings = warnings,
        verbose = verbose,
        scale = scale,
        throttle = throttle
      )
    },
    #' @description Pop a completed task from the results data frame.
    #' @return If there is no task to collect, return `NULL`. Otherwise,
    #'   return a one-row `tibble` with the same columns as `pop()`
    #'   for ordinary controllers.
    #' @param scale Logical, whether to automatically scale workers to meet
    #'   demand. See the `scale` argument of the `pop()` method of
    #'   ordinary single controllers.
    #' @param collect Deprecated in version 0.5.0.9003 (2023-10-02). Not used.
    #' @param throttle `TRUE` to skip auto-scaling if it already happened
    #'   within the last `seconds_interval` seconds. `FALSE` to auto-scale
    #'   every time `scale()` is called. Throttling avoids
    #'   overburdening the `mirai` dispatcher and other resources.
    #' @param error `NULL` or character of length 1, choice of action if
    #'   the popped task threw an error. Possible values:
    #'   * `"stop"`: throw an error in the main R session instead of returning
    #'     a value.
    #'   * `"warn"`: throw a warning.
    #'   * `NULL` or `"silent"`: do not react to errors.
    #' @param controllers Character vector of controller names.
    #'   Set to `NULL` to select all controllers.
    pop = function(
      scale = TRUE,
      collect = NULL,
      throttle = TRUE,
      error = NULL,
      controllers = NULL
    ) {
      control <- private$.select_controllers(controllers)
      for (controller in control) {
        out <- controller$pop(
          scale = scale,
          throttle = throttle,
          error = error
        )
        if (!is.null(out)) {
          return(out)
        }
      }
      NULL
    },
    #' @description Pop all available task results and return them in a tidy
    #'   `tibble`.
    #' @return A `tibble` of results and metadata of all resolved tasks,
    #'   with one row per task. Returns `NULL` if there are no available
    #'   results.
    #' @param scale Logical of length 1,
    #'   whether to automatically call `scale()`
    #'   to auto-scale workers to meet the demand of the task load.
    #' @param throttle `TRUE` to skip auto-scaling if it already happened
    #'   within the last `seconds_interval` seconds. `FALSE` to auto-scale
    #'   every time `scale()` is called. Throttling avoids
    #'   overburdening the `mirai` dispatcher and other resources.
    #' @param error `NULL` or character of length 1, choice of action if
    #'   the popped task threw an error. Possible values:
    #'   * `"stop"`: throw an error in the main R session instead of returning
    #'     a value.
    #'   * `"warn"`: throw a warning.
    #'   * `NULL` or `"silent"`: do not react to errors.
    #' @param controllers Character vector of controller names.
    #'   Set to `NULL` to select all controllers.
    collect = function(
      scale = TRUE,
      throttle = TRUE,
      error = NULL,
      controllers = NULL
    ) {
      control <- private$.select_controllers(controllers)
      out <- map(
        control,
        ~.x$collect(
          scale = scale,
          throttle = throttle,
          error = error
        )
      )
      out <- tibble::new_tibble(data.table::rbindlist(out, use.names = FALSE))
      if_any(nrow(out), out, NULL)
    },
    #' @description Create a `promises::promise()` object to asynchronously
    #'   pop or collect one or more tasks.
    #' @details Please be aware that `pop()` or `collect()` will happen
    #'   asynchronously at a some unpredictable time after the promise object
    #'   is created, even if your local R process appears to be doing
    #'   something completely different. This behavior is highly desirable
    #'   in a Shiny reactive context, but please be careful as it may be
    #'   surprising in other situations.
    #' @return A `promises::promise()` object whose eventual value will
    #'   be a `tibble` with results from one or more popped tasks.
    #'   If `mode = "one"`, only one task is popped and returned (one row).
    #'   If `mode = "all"`, then all the tasks are returned in a `tibble`
    #'   with one row per task (or `NULL` is returned if there are no
    #'   tasks to pop).
    #' @param mode Character of length 1, what kind of promise to create.
    #'   `mode` must be `"one"` or `"all"`. Details:
    #'   * If `mode` is `"one"`, then the promise is fulfilled (or rejected)
    #'     when at least one task is resolved and available to `pop()`.
    #'     When that happens, `pop()` runs asynchronously, pops a result off
    #'     the task list, and returns a value.
    #'     If the task succeeded, then the promise
    #'     is fulfilled and its value is the result of `pop()` (a one-row
    #'     `tibble` with the result and metadata). If the task threw an error,
    #'     the error message of the task is forwarded to any error callbacks
    #'     registered with the promise.
    #'   * If `mode` is `"all"`, then the promise is fulfilled (or rejected)
    #'     when there are no unresolved tasks left in the controller.
    #'     (Be careful: this condition is trivially met in the moment
    #'     if the controller is empty and you have not submitted any tasks,
    #'     so it is best to create this kind of promise only after you
    #'     submit tasks.)
    #'     When there are no unresolved tasks left,
    #'     `collect()` runs asynchronously, pops all available results
    #'     off the task list, and returns a value.
    #'     If the task succeeded, then the promise
    #'     is fulfilled and its value is the result of `collect()`
    #'     (a `tibble` with one row per task result). If any of the tasks
    #'     threw an error, then the first error message detected is forwarded
    #'     to any error callbacks registered with the promise.
    #' @param seconds_interval Positive numeric of length 1, delay in the
    #'   `later::later()` polling interval to asynchronously check if
    #'   the promise can be resolved.
    #' @param scale Deprecated on 2024-04-10 (version 0.9.1.9003)
    #'   and no longer used. Now, `promise()` always turns on auto-scaling
    #'   in a private `later` loop (if not already activated).
    #' @param throttle Deprecated on 2024-04-10 (version 0.9.1.9003)
    #'   and no longer used. Now, `promise()` always turns on auto-scaling
    #'   in a private `later` loop (if not already activated).
    #' @param controllers Not used. Included to ensure the signature is
    #'   compatible with the analogous method of controller groups.
    promise = function(
      mode = "one",
      seconds_interval = 0.1,
      scale = NULL,
      throttle = NULL,
      controllers = NULL
    ) {
      # Tested in tests/interactive/test-promises.R.
      # nocov start
      crew_deprecate(
        name = "controller$promise()",
        date = "2024-04-19",
        version = "> 0.9.2",
        alternative = paste(
          "see https://wlandau.github.io/crew/articles/shiny.html for the",
          "latest on using {crew} in promise-driven Shiny apps"
        ),
        value = TRUE,
        frequency = "once"
      )
      self$autoscale(controllers = controllers)
      controller_promise(
        controller = self,
        mode = mode,
        seconds_interval = seconds_interval,
        controllers = controllers
      )
      # nocov end
    },
    #' @description Wait for tasks.
    #' @details The `wait()` method blocks the calling R session and
    #'   repeatedly auto-scales workers for tasks that need them.
    #'   The function runs until it either times out or the condition
    #'   in `mode` is met.
    #' @return A logical of length 1, invisibly. `TRUE` if the condition
    #'   in `mode` was met, `FALSE` otherwise.
    #' @param mode Character of length 1: `"all"` to wait for
    #'   all tasks in all controllers to complete, `"one"` to wait for
    #'   a single task in a single controller to complete. In this scheme,
    #'   the timeout limit is applied to each controller sequentially,
    #'   and a timeout is treated the same as a completed controller.
    #' @param seconds_interval Deprecated on 2025-01-17 (`crew` version
    #'   0.10.2.9003). Instead, the `seconds_interval` argument passed
    #'   to [crew_controller_group()] is used as `seconds_max`
    #'   in a [crew_throttle()] object which orchestrates exponential
    #'   backoff.
    #' @param seconds_timeout Timeout length in seconds waiting for
    #'   results to become available.
    #' @param scale Logical of length 1, whether to call `scale_later()`
    #'   on each selected controller to schedule auto-scaling.
    #'   See the `scale` argument of the `wait()` method of
    #'   ordinary single controllers.
    #' @param throttle `TRUE` to skip auto-scaling if it already happened
    #'   within the last `seconds_interval` seconds. `FALSE` to auto-scale
    #'   every time `scale()` is called. Throttling avoids
    #'   overburdening the `mirai` dispatcher and other resources.
    #' @param controllers Character vector of controller names.
    #'   Set to `NULL` to select all controllers.
    wait = function(
      mode = "all",
      seconds_interval = NULL,
      seconds_timeout = Inf,
      scale = TRUE,
      throttle = TRUE,
      controllers = NULL
    ) {
      crew_deprecate(
        name = "seconds_interval",
        date = "2025-01-17",
        version = "0.10.2.9003",
        alternative = "none (no longer used)",
        condition = "warning",
        value = seconds_interval
      )
      mode <- as.character(mode)
      crew_assert(mode, identical(., "all") || identical(., "one"))
      control <- private$.select_controllers(controllers)
      out <- if_any(
        identical(mode, "one"),
        private$.wait_one(
          controllers = controllers,
          control = control,
          seconds_timeout = seconds_timeout,
          scale = scale,
          throttle = throttle
        ),
        private$.wait_all(
          controllers = controllers,
          control = control,
          seconds_timeout = seconds_timeout,
          scale = scale,
          throttle = throttle
        )
      )
      invisible(out)
    },
    #' @description Push the name of a task to the backlog.
    #' @details `pop_backlog()` pops the tasks that can be pushed
    #'   without saturating the controller.
    #' @param name Character of length 1 with the task name to push to
    #'   the backlog.
    #' @return `NULL` (invisibly).
    #' @param controller Character vector of length 1 with the controller name.
    #'   Set to `NULL` to select the default controller that `push_backlog()`
    #'   would choose.
    push_backlog = function(name, controller = NULL) {
      control <- .subset2(
        private,
        ".select_single_controller"
      )(name = controller)
      control$push_backlog(name = name)
    },
    #' @description Pop the task names from the head of the backlog which
    #'   can be pushed without saturating the controller.
    #' @return Character vector of task names which can be pushed to the
    #'   controller without saturating it. If the controller is saturated,
    #'   `character(0L)` is returned.
    #' @param controllers Character vector of controller names.
    #'   Set to `NULL` to select all controllers.
    pop_backlog = function(controllers = NULL) {
      control <- private$.select_controllers(controllers)
      out <- map(control, ~.subset2(.x, "pop_backlog")())
      unlist(out, use.names = FALSE)
    },
    #' @description Summarize the workers of one or more controllers.
    #' @return A data frame of aggregated worker summary statistics
    #'   of all the selected controllers. It has one row per worker,
    #'   and the rows are grouped by controller.
    #'   See the documentation of the `summary()` method of the controller
    #'   class for specific information about the columns in the output.
    #' @param controllers Character vector of controller names.
    #'   Set to `NULL` to select all controllers.
    summary = function(controllers = NULL) {
      control <- private$.select_controllers(controllers)
      out <- map(control, ~.x$summary())
      if (all(map_lgl(out, is.null))) {
        return(NULL) # nocov
      }
      tibble::as_tibble(do.call(what = rbind, args = out))
    },
    #' @description Get the process IDs of the local process and the
    #'   `mirai` dispatchers (if started).
    #' @return An integer vector of process IDs of the local process and the
    #'   `mirai` dispatcher (if started).
    #' @param controllers Character vector of controller names.
    #'   Set to `NULL` to select all controllers.
    pids = function(controllers = NULL) {
      control <- private$.select_controllers(controllers)
      out <- map(control, ~.x$pids())
      out <- map(unname(out), ~.x[names(.x) != "local"])
      c(local = Sys.getpid(), unlist(out))
    },
    #' @description Terminate the workers and disconnect the client
    #'   for one or more controllers.
    #' @return `NULL` (invisibly).
    #' @param controllers Character vector of controller names.
    #'   Set to `NULL` to select all controllers.
    terminate = function(controllers = NULL) {
      walk(private$.select_controllers(controllers), ~.x$terminate())
    }
  )
)
