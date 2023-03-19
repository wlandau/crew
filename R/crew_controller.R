#' @title Create a controller.
#' @export
#' @keywords internal
#' @family controllers
#' @description Create an `R6` object to submit tasks and launch workers.
#' @param router An `R6` router object created by [crew_router()].
#' @param launcher An `R6` launcher object created by one of the
#'   `crew_launcher_*()` functions such as [crew_launcher_callr()].
#' @param auto_scale Character of length 1, name of the method for
#'   automatically scaling workers to meet demand. `NULL` to default to
#'   `"demand"`. Possible values include the following:
#'   * `"demand"`: just after pushing a new task in `push()`, launch
#'     `min(n, max(0, t - w))` workers, where `n` is the maximum number of
#'     workers, `t` is the number of queued tasks, and `w` is the current
#'     number of workers already running. In other words, scale up the
#'     number of workers to meet the current demand.
#'     If you trust tasks not to crash workers, this is a good choice.
#'     But if you think a task may always crash a worker
#'     (e.g. segmentation fault or maxed out memory) then
#'     this could be somewhat risky because `mirai` resubmits
#'     failed tasks behind the scenes and `crew` responds by
#'     re-launching workers. If you are worried about this scenario,
#'     choose `auto_scale = "one"` instead, which will only launch
#'     up to one worker whenever a task is pushed.
#'   * `"one"`: just after pushing a new task in `push()`, launch
#'     a one worker if demand `min(n, max(0, t - w))` is greater than 0.
#'   * `"none"`: do not auto-scale at all.
#' @examples
#' if (identical(Sys.getenv("CREW_EXAMPLES"), "true")) {
#' crew_session_start()
#' router <- crew_router()
#' launcher <- crew_launcher_callr()
#' controller <- crew_controller(router = router, launcher = launcher)
#' controller$start()
#' controller$push(name = "task", command = sqrt(4))
#' controller$wait()
#' controller$pop()
#' controller$terminate()
#' crew_session_terminate()
#' }
crew_controller <- function(
  router,
  launcher,
  auto_scale = "demand"
) {
  auto_scale <- auto_scale %|||% c("demand", "one", "none")
  controller <- crew_class_controller$new(
    router = router,
    launcher = launcher,
    auto_scale = auto_scale
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
#' crew_session_start()
#' router <- crew_router()
#' launcher <- crew_launcher_callr()
#' controller <- crew_controller(router = router, launcher = launcher)
#' controller$start()
#' controller$push(name = "task", command = sqrt(4))
#' controller$wait()
#' controller$pop()
#' controller$terminate()
#' crew_session_terminate()
#' }
crew_class_controller <- R6::R6Class(
  classname = "crew_class_controller",
  public = list(
    #' @field router Router object.
    router = NULL,
    #' @field launcher Launcher object.
    launcher = NULL,
    #' @field auto_scale Scaling method. See [crew_controller()].
    auto_scale = NULL,
    #' @field queue List of tasks in the queue.
    queue = list(),
    #' @field results List of finished tasks
    results = list(),
    #' @field log Data frame task log of the workers.
    log = NULL,
    #' @description `mirai` controller constructor.
    #' @return An `R6` object with the controller object.
    #' @param router Router object. See [crew_controller()].
    #' @param launcher Launcher object. See [crew_controller()].
    #' @param auto_scale Scaling method. See [crew_controller()].
    #' @examples
    #' if (identical(Sys.getenv("CREW_EXAMPLES"), "true")) {
    #' crew_session_start()
    #' router <- crew_router()
    #' launcher <- crew_launcher_callr()
    #' controller <- crew_controller(router = router, launcher = launcher)
    #' controller$start()
    #' controller$push(name = "task", command = sqrt(4))
    #' controller$wait()
    #' controller$pop()
    #' controller$terminate()
    #' crew_session_terminate()
    #' }
    initialize = function(
    router = NULL,
    launcher = NULL,
    auto_scale = NULL
    ) {
      self$router <- router
      self$launcher <- launcher
      self$auto_scale <- auto_scale
      invisible()
    },
    #' @description Validate the router.
    #' @return `NULL` (invisibly).
    validate = function() {
      true(is.list(self$queue))
      true(is.list(self$results))
      true(self$log, is.null(.) %|||% is.data.frame(.))
      true(inherits(self$router, "crew_class_router"))
      true(inherits(self$launcher, "crew_class_launcher"))
      self$router$validate()
      self$launcher$validate()
      invisible()
    },
    #' @description Start the controller if it is not already started.
    #' @details Register the mirai client and register worker websockets
    #'   with the launcher.
    #' @return `NULL` (invisibly).
    #' @param controllers Not used. Included to ensure the signature is
    #'   compatible with the analogous method of controller groups.
    start = function(controllers = NULL) {
      if (!self$router$listening()) {
        self$router$listen()
        sockets <- self$router$daemons$worker_socket
        self$launcher$populate(sockets = sockets)
        self$log <- tibble::tibble(
          worker_socket = sockets,
          popped_tasks = rep(0L, length(sockets)),
          popped_seconds = rep(0, length(sockets)),
          popped_errors = rep(0L, length(sockets)),
          popped_warnings = rep(0L, length(sockets)),
          controller = rep(self$router$name, length(sockets))
        )
      }
      invisible()
    },
    #' @description Force terminate workers whose startup time has elapsed
    #'   and are not connected to the `mirai` client.
    #' @return `NULL` (invisibly).
    clean = function() {
      inactive <- setdiff(
        self$router$daemons$worker_socket,
        self$launcher$active()
      )
      self$launcher$terminate(sockets = inactive)
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
      inactive <- setdiff(
        self$router$daemons$worker_socket,
        self$launcher$active()
      )
      sockets <- utils::head(inactive, n = n)
      if (length(sockets) > 0L) {
        self$launcher$launch(sockets = sockets)
      }
      invisible()
    },
    #' @description Run auto-scaling.
    #' @details This method is called during `push()`, and the method for
    #'   scaling up workers is governed by the `auto_scale`
    #'   argument of [crew_controller()]. It is not meant to be called
    #'   manually. If called manually, it is recommended to call `collect()`
    #'   first so `scale()` can accurately assess the demand.
    #'   For finer control of the number of workers launched,
    #'   call `launch()` on the controller with the exact desired
    #'   number of workers.
    #' @return `NULL` (invisibly).
    #' @param controllers Not used. Included to ensure the signature is
    #'   compatible with the analogous method of controller groups.
    scale = function(controllers = NULL) {
      demand <- controller_demand(
        tasks = length(self$queue),
        workers = length(self$launcher$active())
      )
      n_new_workers <- controller_n_new_workers(
        demand = demand,
        auto_scale = self$auto_scale
      )
      if (n_new_workers > 0L) {
        self$launch(n = n_new_workers)
      }
      invisible()
    },
    #' @description Check for done tasks and move the results to
    #'   the results list.
    #' @return `NULL` (invisibly). Removes elements from the `queue`
    #'   list as applicable and moves them to the `results` list.
    #' @param controllers Not used. Included to ensure the signature is
    #'   compatible with the analogous method of controller groups.
    collect = function(controllers = NULL) {
      done <- integer(0L)
      for (index in seq_along(self$queue)) {
        task <- self$queue[[index]]
        if (!nanonext_unresolved(task$handle[[1L]])) {
          self$results[[length(self$results) + 1L]] <- task
          done[length(done) + 1L] <- index
        }
      }
      self$queue[done] <- NULL
      invisible()
    },
    #' @description Push a task to the head of the task list.
    #' @return `NULL` (invisibly).
    #' @param command Language object with R code to run.
    #' @param data Named list of local data objects in the
    #'   evaluation environment.
    #' @param globals Named list of objects to temporarily assign to the
    #'   global environment for the task. At the end of the task,
    #'   these values are reset to their previous values.
    #' @param seed Integer of length 1 with the pseudo-random number generator
    #'   seed to temporarily set for the evaluation of the task.
    #'   At the end of the task, the seed is restored.
    #' @param garbage_collection Logical, whether to run garbage collection
    #'   with `gc()` before running the task.
    #' @param packages Character vector of packages to load for the task.
    #' @param library Library path to load the packages. See the `lib.loc`
    #'   argument of `require()`.
    #' @param name Optional name of the task. Replaced with a random name
    #'   if `NULL` or in conflict with an existing name in the task list.
    #' @param seconds_timeout Optional task timeout passed to the `.timeout`
    #'   argument of `mirai::mirai()` (after converting to milliseconds).
    #' @param scale Logical, whether to automatically scale workers to meet
    #'   demand. If `TRUE`, then `clean()` and `collect()` run first
    #'   so demand can be properly assessed before scaling and the number
    #'   of workers is not too high.
    #' @param controller Not used. Included to ensure the signature is
    #'   compatible with the analogous method of controller groups.
    push = function(
      command,
      data = list(),
      globals = list(),
      seed = sample.int(n = 1e9L, size = 1L),
      garbage_collection = FALSE,
      packages = character(0),
      library = NULL,
      seconds_timeout = NULL,
      name = NULL,
      scale = TRUE,
      controller = NULL
    ) {
      true(scale, isTRUE(.) || isFALSE(.))
      while (is.null(name) || name %in% self$queue$name) name <- random_name()
      command <- substitute(command)
      string <- deparse_safe(command)
      command <- rlang::call2("quote", command)
      expr <- rlang::call2(
        .fn = quote(crew::crew_eval),
        command = command,
        data = quote(data),
        globals = quote(globals),
        seed = quote(seed),
        garbage_collection = quote(garbage_collection),
        packages = quote(packages),
        library = quote(library)
      )
      .timeout <- if_any(
        is.null(seconds_timeout),
        NULL,
        seconds_timeout * 1000
      )
      mirai_args <- list(
        .expr = expr,
        data = data,
        globals = globals,
        seed = seed,
        garbage_collection = garbage_collection,
        packages = packages,
        library = library,
        .timeout = .timeout,
        .compute = self$router$name
      )
      handle <- do.call(what = mirai::mirai, args = mirai_args)
      task <- list(
        name = name,
        command = string,
        handle = list(handle)
      )
      self$queue[[length(self$queue) + 1L]] <- task
      self$collect()
      if (scale && (length(self$launcher$active()) < self$router$workers)) {
        self$clean()
        self$scale()
      }
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
    #' @param scale Logical, whether to automatically scale workers to meet
    #'   demand. If `TRUE`, then `clean()` and `collect()` run first
    #'   so demand can be properly assessed before scaling and the number
    #'   of workers is not too high. Scaling up on `pop()` may be important
    #'   for transient or nearly transient workers that tend to drop off
    #'   quickly after doing little work.
    #' @param controllers Not used. Included to ensure the signature is
    #'   compatible with the analogous method of controller groups.
    pop = function(scale = TRUE, controllers = NULL) {
      self$collect()
      if (scale && (length(self$launcher$active()) < self$router$workers)) {
        self$clean()
        self$scale()
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
        if (!is.na(out$socket_data)) {
          index <- which(out$socket_data == self$log$worker_socket)
          self$log$popped_tasks[index] <- self$log$popped_tasks[index] + 1L
          self$log$popped_seconds[index] <- self$log$popped_seconds[index] +
            out$seconds
          self$log$popped_errors[index] <- self$log$popped_errors[index] +
            !anyNA(out$error)
          self$log$popped_warnings[index] <-
            self$log$popped_warnings[index] + !anyNA(out$error)
        }
        self$results[[1]] <- NULL
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
    #' @param controllers Not used. Included to ensure the signature is
    #'   compatible with the analogous method of controller groups.
    wait = function(
      mode = "all",
      seconds_interval = 0.001,
      seconds_timeout = Inf,
      controllers = NULL
    ) {
      mode <- as.character(mode)
      true(mode, identical(., "all") || identical(., "one"))
      tryCatch(
        crew_wait(
          fun = ~{
            self$collect()
            self$clean()
            self$scale()
            done <- length(self$results) > 0L
            if (identical(mode, "all")) {
              done <- done && (length(self$queue) < 1L)
            }
            done
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
    #'   * `worker_socket` full websocket address of the worker, including
    #'     the protocol, IP address, TCP port, and path.
    #'     To identify specific pieces of the websocket address,
    #'     call `nanonext::parse_url()`.
    #'   * `worker_connected`: `TRUE` if a worker is currently connected
    #'     to the websocket, `FALSE` if not connected, or `NA`
    #'     if the status cannot be determined because the `mirai`
    #'     client is not running.
    #'   * `worker_busy`: `TRUE` if a worker is currently busy running a
    #'     task, `FALSE` if not busy, or `NA`
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
    #'     of the controller object. For persistent workers that
    #'     always stay running, `worker_instances` will be no more than 1.
    #'     However, in the case of transient workers, e.g. `tasks_max = 1`
    #'     or a small value of `seconds_idle`, worker processes may
    #'     time out or exit when the task workload subsides. Then when the
    #'     task workload surges again, different workers may be launch
    #'     and connect to the available websockets, so `worker_instances`
    #'     could be greater than 1.
    #'   * `tasks_assigned`: number of pushed tasks assigned to the
    #'      current worker process at the websocket. The counter resets
    #'      every time a new worker replaces the previous worker at the
    #'      websocket. So in the case of transient
    #'      workers, this number may be much smaller than the number of
    #'      popped tasks.
    #'   * `tasks_complete`: number of pushed tasks completed by the
    #'      current worker process at the websocket. The counter resets
    #'      every time a new worker replaces the previous worker at the
    #'      websocket. So in the case of transient
    #'      workers, this number may be much smaller than the number of
    #'      popped tasks.
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
    #' @param columns Tidyselect expression to select a subset of columns.
    #'   Examples include `columns = contains("worker")` and
    #'   `columns = starts_with("tasks")`.
    #' @param controllers Not used. Included to ensure the signature is
    #'   compatible with the analogous method of controller groups.
    summary = function(
      columns = tidyselect::everything(),
      controllers = NULL
    ) {
      self$router$poll()
      daemons <- self$router$daemons
      workers <- self$launcher$workers
      log <- self$log
      if (is.null(daemons) || is.null(workers) || is.null(log)) {
        return(NULL)
      }
      out <- tibble::tibble(
        controller = self$router$name,
        worker_socket = daemons$worker_socket,
        worker_connected = daemons$worker_connected,
        worker_busy = daemons$worker_busy,
        worker_launches = workers$launches,
        worker_instances = daemons$worker_instances,
        tasks_assigned = daemons$tasks_assigned,
        tasks_complete = daemons$tasks_complete,
        popped_tasks = log$popped_tasks,
        popped_seconds = log$popped_seconds,
        popped_errors = log$popped_errors,
        popped_warnings = log$popped_warnings
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

controller_demand <- function(tasks, workers) {
  max(0L, tasks - workers)
}

controller_n_new_workers <- function(demand, auto_scale) {
  switch(
    auto_scale,
    demand = demand,
    one = min(1L, demand),
    none = 0L
  ) %|||% 0L
}
