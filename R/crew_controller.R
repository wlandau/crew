#' @title Create a controller.
#' @export
#' @keywords internal
#' @family controllers
#' @description Create an `R6` object to submit tasks and launch workers.
#' @param router An `R6` router object created by [crew_router()].
#' @param launcher An `R6` launcher object created by one of the
#'   `crew_launcher_*()` functions such as [crew_launcher_local()].
#' @param auto_scale Deprecated. Use the `scale` argument of `push()`,
#'   `pop()`, and `wait()` instead.
#' @examples
#' if (identical(Sys.getenv("CREW_EXAMPLES"), "true")) {
#' router <- crew_router()
#' launcher <- crew_launcher_local()
#' controller <- crew_controller(router = router, launcher = launcher)
#' controller$start()
#' controller$push(name = "task", command = sqrt(4))
#' controller$wait()
#' controller$pop()
#' controller$terminate()
#' }
crew_controller <- function(
  router,
  launcher,
  auto_scale = NULL
) {
  if (!is.null(auto_scale)) {
    crew_deprecate(
      name = "auto_scale",
      date = "2023-05-18",
      version = "0.2.0",
      alternative = "use the scale argument of push(), pop(), and wait()"
    )
  }
  controller <- crew_class_controller$new(
    router = router,
    launcher = launcher
  )
  controller$validate()
  controller
}

#' @title Controller class
#' @export
#' @family controllers
#' @description `R6` class for controllers.
#' @details See [crew_controller()].
#' @examples
#' if (identical(Sys.getenv("CREW_EXAMPLES"), "true")) {
#' router <- crew_router()
#' launcher <- crew_launcher_local()
#' controller <- crew_controller(router = router, launcher = launcher)
#' controller$start()
#' controller$push(name = "task", command = sqrt(4))
#' controller$wait()
#' controller$pop()
#' controller$terminate()
#' }
crew_class_controller <- R6::R6Class(
  classname = "crew_class_controller",
  cloneable = FALSE,
  private = list(
    inactive = function() {
      daemons <- self$router$daemons
      launching <- self$launcher$launching()
      which(is_inactive(daemons = daemons, launching = launching))
    },
    scalable = function() {
      daemons <- self$router$daemons
      launching <- self$launcher$launching()
      inactive <- is_inactive(daemons = daemons, launching = launching)
      backlogged <- self$router$assigned > self$router$complete
      list(
        backlogged = which(inactive & backlogged),
        resolved = which(inactive & (!backlogged)),
        n_inactive = sum(inactive)
      )
    },
    try_launch = function(inactive, n) {
      inactive <- utils::head(inactive, n = max(0L, n))
      for (index in inactive) {
        socket <- self$router$route(index = index, force = FALSE)
        self$launcher$launch(index = index, socket = socket)
      }
    }
  ),
  public = list(
    #' @field router Router object.
    router = NULL,
    #' @field launcher Launcher object.
    launcher = NULL,
    #' @field queue List of tasks in the queue.
    queue = list(),
    #' @field results List of finished tasks
    results = list(),
    #' @field log Data frame task log of the workers.
    log = NULL,
    #' @field until_collect Numeric of length 1, time point when
    #'   throttled task collection unlocks.
    until_collect = NULL,
    #' @field until_scale Numeric of length 1, time point when
    #'   throttled auto-scaling unlocks.
    until_scale = NULL,
    #' @description `mirai` controller constructor.
    #' @return An `R6` object with the controller object.
    #' @param router Router object. See [crew_controller()].
    #' @param launcher Launcher object. See [crew_controller()].
    #' @examples
    #' if (identical(Sys.getenv("CREW_EXAMPLES"), "true")) {
    #' router <- crew_router()
    #' launcher <- crew_launcher_local()
    #' controller <- crew_controller(router = router, launcher = launcher)
    #' controller$start()
    #' controller$push(name = "task", command = sqrt(4))
    #' controller$wait()
    #' controller$pop()
    #' controller$terminate()
    #' }
    initialize = function(
      router = NULL,
      launcher = NULL
    ) {
      self$router <- router
      self$launcher <- launcher
      invisible()
    },
    #' @description Validate the router.
    #' @return `NULL` (invisibly).
    validate = function() {
      crew_assert(is.list(self$queue))
      crew_assert(is.list(self$results))
      crew_assert(self$log, is.null(.) || is.data.frame(.))
      crew_assert(inherits(self$router, "crew_class_router"))
      crew_assert(inherits(self$launcher, "crew_class_launcher"))
      self$router$validate()
      self$launcher$validate()
      invisible()
    },
    #' @description Check if the controller is empty.
    #' @details A controller is empty if it has no running tasks
    #'   or completed tasks waiting to be retrieved with `push()`.
    #' @return `TRUE` if the controller is empty, `FALSE` otherwise.
    #' @param controllers Not used. Included to ensure the signature is
    #'   compatible with the analogous method of controller groups.
    empty = function(controllers = NULL) {
      (length(self$queue) < 1L) && (length(self$results) < 1L)
    },
    #' @description Check if the controller is saturated.
    #' @details A controller is saturated if the number of unresolved tasks
    #'   is greater than or equal to the maximum number of workers.
    #'   In other words, in a saturated controller, every available worker
    #'   has a task.
    #'   You can still push tasks to a saturated controller, but
    #'   tools that use `crew` such as `targets` may choose not to.
    #' @return `TRUE` if the controller is saturated, `FALSE` otherwise.
    #' @param collect Logical of length 1, whether to collect the results
    #'   of any newly resolved tasks before determining saturation.
    #' @param throttle Logical of length 1, whether to delay task collection
    #'   until the next request at least `self$router$seconds_interval`
    #'   seconds from the original request.
    #'   The idea is similar to `shiny::throttle()` except that `crew` does not
    #'   accumulate a backlog of requests. The technique improves robustness
    #'   and efficiency.
    #' @param controller Not used. Included to ensure the signature is
    #'   compatible with the analogous method of controller groups.
    saturated = function(collect = TRUE, throttle = TRUE, controller = NULL) {
      if (collect) {
        self$collect(throttle = throttle)
      }
      length(self$queue) >= self$router$workers
    },
    #' @description Start the controller if it is not already started.
    #' @details Register the mirai client and register worker websockets
    #'   with the launcher.
    #' @return `NULL` (invisibly).
    #' @param controllers Not used. Included to ensure the signature is
    #'   compatible with the analogous method of controller groups.
    start = function(controllers = NULL) {
      if (!isTRUE(self$router$started)) {
        self$router$start()
        workers <- self$router$workers
        self$launcher$start(workers = workers)
        self$log <- tibble::tibble(
          popped_tasks = rep(0L, workers),
          popped_seconds = rep(0, workers),
          popped_errors = rep(0L, workers),
          popped_warnings = rep(0L, workers),
          controller = rep(self$router$name, workers)
        )
      }
      invisible()
    },
    #' @description Launch one or more workers.
    #' @return `NULL` (invisibly).
    #' @param n Number of workers to try to launch. The actual
    #'   number launched is capped so that no more than "`workers`"
    #'   workers running at a given time, where "`workers`"
    #'   is an argument of [crew_controller()]. The
    #'   actual cap is the "`workers`" argument minus the number of connected
    #'   workers minus the number of starting workers. A "connected"
    #'   worker has an active websocket connection to the `mirai` client,
    #'   and "starting" means that the worker was launched at most
    #'   `seconds_start` seconds ago, where `seconds_start` is
    #'   also an argument of [crew_controller()].
    #' @param controllers Not used. Included to ensure the signature is
    #'   compatible with the analogous method of controller groups.
    launch = function(n = 1L, controllers = NULL) {
      self$router$poll()
      nanonext::msleep(10)
      self$router$poll()
      self$router$tally()
      inactive <- private$inactive()
      private$try_launch(inactive = inactive, n = n)
      invisible()
    },
    #' @description Run auto-scaling.
    #' @details Methods `push()`, `pop()`, and `wait()` already invoke
    #'   `scale()` if the `scale` argument is `TRUE`.
    #'   If you call `scale()` manually, it is recommended to call `collect()`
    #'   first so `scale()` can accurately assess the task load.
    #'   For finer control of the number of workers launched,
    #'   call `launch()` on the controller with the exact desired
    #'   number of workers.
    #' @return `NULL` (invisibly).
    #' @param throttle Logical of length 1, whether to delay auto-scaling
    #'   until the next auto-scaling request at least
    #'  `self$router$seconds_interval` seconds from the original request.
    #'   The idea is similar to `shiny::throttle()` except that `crew` does not
    #'   accumulate a backlog of requests. The technique improves robustness
    #'   and efficiency.
    #' @param controllers Not used. Included to ensure the signature is
    #'   compatible with the analogous method of controller groups.
    scale = function(throttle = FALSE, controllers = NULL) {
      if (throttle) {
        now <- nanonext::mclock()
        if (is.null(self$until_scale)) {
          self$until_scale <- now + (1000 * self$router$seconds_interval)
        }
        if (now < self$until_scale) {
          return(invisible())
        } else {
          self$until_scale <- NULL
        }
      }
      self$router$poll()
      nanonext::msleep(10)
      self$router$poll()
      self$router$tally()
      scalable <- private$scalable()
      private$try_launch(inactive = scalable$backlogged, n = Inf)
      available <- self$router$workers - length(scalable$resolved)
      self$collect(throttle = FALSE)
      deficit <- min(length(self$queue) - available, self$router$workers)
      private$try_launch(inactive = scalable$resolved, n = deficit)
      invisible()
    },
    #' @description Push a task to the head of the task list.
    #' @return `NULL` (invisibly).
    #' @param command Language object with R code to run.
    #' @param data Named list of local data objects in the
    #'   evaluation environment.
    #' @param globals Named list of objects to temporarily assign to the
    #'   global environment for the task. See the `reset_globals` argument
    #'   of [crew_controller_local()].
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
    #'   seed to temporarily set for the evaluation of the task.
    #'   At the end of the task, the seed is restored.
    #' @param packages Character vector of packages to load for the task.
    #' @param library Library path to load the packages. See the `lib.loc`
    #'   argument of `require()`.
    #' @param seconds_timeout Optional task timeout passed to the `.timeout`
    #'   argument of `mirai::mirai()` (after converting to milliseconds).
    #' @param scale Logical, whether to automatically call `scale()`
    #'   to auto-scale workers to meet the demand of the task load.
    #'   By design, auto-scaling might not actually happen
    #'   if `throttle = TRUE`.
    #'   If `scale` is `TRUE`, then `collect()` runs first
    #'   so demand can be properly assessed before scaling and the number
    #'   of workers is not too high.
    #' @param throttle If `scale` is `TRUE`, whether to defer auto-scaling
    #'   until the next request at least
    #'   `self$router$seconds_interval` seconds from the original request.
    #'   The idea is similar to `shiny::throttle()` except that `crew` does not
    #'   accumulate a backlog of requests. The technique improves robustness
    #'   and efficiency.
    #' @param name Optional name of the task. Replaced with a random name
    #'   if `NULL` or in conflict with an existing name in the task list.
    #' @param controller Not used. Included to ensure the signature is
    #'   compatible with the analogous method of controller groups.
    push = function(
      command,
      data = list(),
      globals = list(),
      substitute = TRUE,
      seed = sample.int(n = 1e9L, size = 1L),
      packages = character(0),
      library = NULL,
      seconds_timeout = NULL,
      scale = TRUE,
      throttle = TRUE,
      name = NULL,
      controller = NULL
    ) {
      crew_assert(scale, isTRUE(.) || isFALSE(.))
      while (is.null(name) || name %in% self$queue$name) {
        name <- crew_random_name()
      }
      if (substitute) {
        command <- substitute(command)
      }
      string <- deparse_safe(command)
      command <- rlang::call2("quote", command)
      expr <- rlang::call2(
        .fn = quote(crew::crew_eval),
        command = command,
        data = quote(data),
        globals = quote(globals),
        seed = quote(seed),
        packages = quote(packages),
        library = quote(library)
      )
      .args <- list(
        data = data,
        globals = globals,
        seed = seed,
        packages = packages,
        library = library
      )
      .timeout <- if_any(
        is.null(seconds_timeout),
        NULL,
        seconds_timeout * 1000
      )
      handle <- mirai::mirai(
        .expr = expr,
        .args = .args,
        .timeout = .timeout,
        .compute = self$router$name
      )
      task <- list(
        name = name,
        command = string,
        handle = list(handle)
      )
      self$queue[[length(self$queue) + 1L]] <- task
      if (scale) {
        self$scale(throttle = throttle)
      }
      invisible()
    },
    #' @description Check for done tasks and move the results to
    #'   the results list.
    #' @return `NULL` (invisibly). Removes elements from the `queue`
    #'   list as applicable and moves them to the `results` list.
    #' @param throttle whether to defer task collection
    #'   until the next task collection request at least
    #'   `self$router$seconds_interval` seconds from the original request.
    #'   The idea is similar to `shiny::throttle()` except that `crew` does not
    #'   accumulate a backlog of requests. The technique improves robustness
    #'   and efficiency.
    #' @param controllers Not used. Included to ensure the signature is
    #'   compatible with the analogous method of controller groups.
    collect = function(throttle = FALSE, controllers = NULL) {
      if (throttle) {
        now <- nanonext::mclock()
        if (is.null(self$until_collect)) {
          self$until_collect <- now + (1000 * self$router$seconds_interval)
        }
        if (now < self$until_collect) {
          return(invisible())
        } else {
          self$until_collect <- NULL
        }
      }
      done <- rep(FALSE, length(self$queue))
      for (index in seq_along(done)) {
        task <- self$queue[[index]]
        if (!nanonext::.unresolved(task$handle[[1L]])) {
          self$results[[length(self$results) + 1L]] <- task
          done[index] <- TRUE
        }
      }
      self$queue[done] <- NULL
      invisible()
    },
    #' @description Pop a completed task from the results data frame.
    #' @details If not task is currently completed and collected, `pop()`
    #'   will attempt to auto-scale workers as needed and collect
    #'   any newly completed results.
    #' @return If there is a completed task available to collect, the return
    #'   value is a one-row data frame with the results, warnings, and errors.
    #'   Otherwise, if there are no results available to collect,
    #'   the return value is `NULL`.
    #' @param scale Logical of length 1,
    #'   whether to automatically call `scale()`
    #'   to auto-scale workers to meet the demand of the task load.
    #'   Auto-scaling might not actually happen if `throttle` is `TRUE`.
    #'   If `TRUE`, then `collect()` runs first
    #'   so demand can be properly assessed before scaling and the number
    #'   of workers is not too high. Scaling up on `pop()` may be important
    #'   for transient or nearly transient workers that tend to drop off
    #'   quickly after doing little work.
    #' @param collect Logical of length 1. If `scale` is `FALSE`,
    #'   whether to call `collect()`
    #'   to pick up the results of completed tasks. This task collection
    #'   step always happens (with throttling) when `scale` is `TRUE`.
    #' @param throttle Whether to defer auto-scaling and task collection
    #'   until the next request at least
    #'   `self$router$seconds_interval` seconds from the original request.
    #'   The idea is similar to `shiny::throttle()` except that `crew` does not
    #'   accumulate a backlog of requests. The technique improves robustness
    #'   and efficiency.
    #' @param controllers Not used. Included to ensure the signature is
    #'   compatible with the analogous method of controller groups.
    pop = function(
      scale = TRUE,
      collect = TRUE,
      throttle = TRUE,
      controllers = NULL
    ) {
      if (scale) {
        self$scale(throttle = throttle)
      } else if (collect) {
        self$collect(throttle = throttle)
      }
      out <- NULL
      if (length(self$results) > 0L) {
        task <- self$results[[1L]]
        out <- task$handle[[1L]]$data
        # The contents of the if() statement below happen
        # if mirai cannot evaluate the command.
        # I cannot cover this in automated tests, but
        # I did test it by hand.
        # nocov start
        if (!inherits(out, "crew_monad")) {
          out <- monad_init(
            command = task$command,
            error = paste(
              utils::capture.output(print(out), type = "output"),
              collapse = "\n"
            )
          )
        }
        # nocov end
        out$name <- task$name
        out <- tibble::new_tibble(as.list(out))
        if (!is.na(out$launcher)) {
          index <- out$worker
          self$log$popped_tasks[index] <- self$log$popped_tasks[index] + 1L
          self$log$popped_seconds[index] <- self$log$popped_seconds[index] +
            out$seconds
          self$log$popped_errors[index] <- self$log$popped_errors[index] +
            !anyNA(out$error)
          self$log$popped_warnings[index] <-
            self$log$popped_warnings[index] + !anyNA(out$error)
        }
        self$results[[1L]] <- NULL
      }
      out
    },
    #' @description Wait for tasks.
    #' @details The `wait()` method blocks the calling R session,
    #'   repeatedly auto-scales workers for tasks
    #'   that need them, and repeatedly collects results.
    #'   The function runs until it either times out or reaches
    #'   its stopping condition based on the `mode` argument.
    #' @return `NULL` (invisibly). Call `pop()` to get the result.
    #' @param mode If `mode` is `"all"`,
    #'   then the method waits for all tasks to complete. If `mode` is
    #'   `"one"`, then it waits until a one task is complete.
    #' @param seconds_interval Number of seconds to wait between polling
    #'   intervals waiting for tasks.
    #' @param seconds_timeout Timeout length in seconds waiting for tasks.
    #' @param scale Logical, whether to automatically call `scale()`
    #'   to auto-scale workers to meet the demand of the task load.
    #'   Might not actually auto-scale on every iteration if `throttle`
    #'   is `TRUE`.
    #' @param throttle Whether to defer auto-scaling
    #'   and task collection until the next request at least
    #'   `self$router$seconds_interval` seconds from the original request.
    #'   The idea is similar to `shiny::throttle()` except that `crew` does not
    #'   accumulate a backlog of requests. The technique improves robustness
    #'   and efficiency.
    #'   Highly recommended
    #'   to keep `throttle = TRUE` in `wait()` to ensure efficiency
    #'   and robustness.
    #' @param controllers Not used. Included to ensure the signature is
    #'   compatible with the analogous method of controller groups.
    wait = function(
      mode = "all",
      seconds_interval = 0.01,
      seconds_timeout = Inf,
      scale = TRUE,
      throttle = TRUE,
      controllers = NULL
    ) {
      mode <- as.character(mode)
      crew_assert(mode, identical(., "all") || identical(., "one"))
      tryCatch(
        crew_retry(
          fun = ~{
            if_any(
              scale,
              self$scale(throttle = throttle),
              self$collect(throttle = throttle)
            )
            empty_queue <- length(self$queue) < 1L
            empty_results <- length(self$results) < 1L
            (empty_queue && empty_results) || if_any(
              identical(mode, "all"),
              empty_queue && (!empty_results),
              empty_queue || (!empty_results)
            )
          },
          seconds_interval = seconds_interval,
          seconds_timeout = seconds_timeout
        ),
        crew_expire = function(condition) NULL
      )
      invisible()
    },
    #' @description Summarize the workers and tasks of the controller.
    #' @return A data frame of summary statistics on the workers and tasks.
    #'   It has one row per worker websocket and the following columns:
    #'   * `controller`: name of the controller.
    #'   * `popped_tasks`: number of tasks which were completed by
    #'     a worker at the websocket and then returned by calling
    #'     `pop()` on the controller object.
    #'   * `popped_seconds`: total number of runtime and seconds of
    #'     all the tasks that ran on a worker connected to this websocket
    #'     and then were retrieved by calling `pop()` on the controller
    #'     object.
    #'   * `popped_errors`: total number of tasks which ran on a worker
    #'     at the website, encountered an error in R, and then retrieved
    #'     with `pop()`.
    #'   * `popped_warnings`: total number of tasks which ran on a worker
    #'     at the website, encountered one or more warnings in R,
    #'     and then retrieved with `pop()`. Note: `popped_warnings`
    #'     is actually the number of *tasks*, not the number of warnings.
    #'     (A task could throw more than one warning.)
    #'   * `tasks_assigned`: number of pushed tasks assigned to the
    #'      current worker process at the websocket. The counter resets
    #'      every time a new worker instance starts.
    #'      So in the case of transient
    #'      workers, this number may be much smaller than the number of
    #'      popped tasks.
    #'   * `tasks_complete`: number of pushed tasks completed by the
    #'      current worker process at the websocket. The counter resets
    #'      every time a new worker instance starts.
    #'      So in the case of transient
    #'      workers, this number may be much smaller than the number of
    #'      popped tasks.
    #'   * `worker_index`: Numeric index of the worker within the controller.
    #'   * `worker_connected`: `TRUE` if a worker is currently connected
    #'     to the websocket, `FALSE` if not connected, or `NA`
    #'     if the status cannot be determined because the `mirai`
    #'     client is not running.
    #'   * `worker_launches`: number of attempts to launch a worker
    #'     at the websocket since the controller started. If
    #'     the number of launch attempts gets much higher than
    #'     the number of popped tasks or worker instances, then this is a
    #'     sign that something is wrong with the workers or platform,
    #'     and it is recommended to quit the pipeline and troubleshoot.
    #'   * `worker_instances`: number of different worker processes
    #'     that have connected to the websocket since the `start()`
    #'     of the controller object. Should either be 0 or 1 unless
    #'     something is wrong and more than one worker has connected
    #'     to the current websocket.
    #'   * `worker_socket` full websocket address of the worker, including
    #'     the protocol, IP address, TCP port, and path.
    #'     This websocket rotates with every additional instance
    #'     of a worker process.
    #'     To identify specific pieces of the websocket address,
    #'     call `nanonext::parse_url()`.
    #' @param columns Tidyselect expression to select a subset of columns.
    #'   Examples include `columns = contains("worker")` and
    #'   `columns = starts_with("tasks")`.
    #' @param controllers Not used. Included to ensure the signature is
    #'   compatible with the analogous method of controller groups.
    summary = function(
      columns = tidyselect::everything(),
      controllers = NULL
    ) {
      router_log <- self$router$log()
      workers <- self$launcher$workers
      log <- self$log
      if (is.null(router_log) || is.null(workers) || is.null(log)) {
        return(NULL)
      }
      out <- tibble::tibble(
        controller = self$router$name,
        popped_tasks = log$popped_tasks,
        popped_seconds = log$popped_seconds,
        popped_errors = log$popped_errors,
        popped_warnings = log$popped_warnings,
        tasks_assigned = router_log$tasks_assigned,
        tasks_complete = router_log$tasks_complete,
        worker_index = seq_len(nrow(workers)),
        worker_connected = router_log$worker_connected,
        worker_launches = workers$launches,
        worker_instances = router_log$worker_instances,
        worker_socket = router_log$worker_socket
      )
      expr <- rlang::enquo(columns)
      select <- eval_tidyselect(expr = expr, choices = colnames(out))
      out[, select, drop = FALSE]
    },
    #' @description Terminate the workers and the `mirai` client.
    #' @return `NULL` (invisibly).
    #' @param controllers Not used. Included to ensure the signature is
    #'   compatible with the analogous method of controller groups.
    terminate = function(controllers = NULL) {
      # https://github.com/r-lib/covr/issues/315 # nolint
      terminate_launcher_first <- identical(Sys.getenv("R_COVR"), "true") ||
        identical(tolower(Sys.info()[["sysname"]]), "windows")
      # nocov start
      if (terminate_launcher_first) {
        self$launcher$terminate()
        self$router$terminate()
      } else {
        self$router$terminate()
        self$launcher$terminate()
      }
      # nocov end
      invisible()
    }
  )
)

is_inactive <- function(daemons, launching) {
  connected <- as.logical(daemons[, "online"] > 0L)
  discovered <- as.logical(daemons[, "instance"] > 0L)
  (!connected) & (discovered | (!launching))
}
