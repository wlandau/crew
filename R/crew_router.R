#' @title Create a router.
#' @export
#' @keywords internal
#' @family routers
#' @description Create an `R6` object to manage the `mirai` task scheduler
#'   client.
#' @param name Name of the router object. If `NULL`, a name is automatically
#'   generated.
#' @param workers Integer, maximum number of parallel workers to run.
#' @param host IP address of the `mirai` client to send and receive tasks.
#'   If `NULL`, the host defaults to the local IP address.
#' @param port TCP port to listen for the workers. If `NULL`,
#'   then an available ephemeral port is automatically chosen.
#' @param seconds_interval Number of seconds between
#'   polling intervals waiting for certain internal
#'   synchronous operations to complete. If `space_poll` is `TRUE`, then
#'   this is also the minimum number of seconds between calls to
#'   `mirai::daemons()` for the purposes of checking worker status.
#' @param seconds_timeout Number of seconds until timing
#'   out while waiting for certain synchronous operations to complete.
#' @examples
#' if (identical(Sys.getenv("CREW_EXAMPLES"), "true")) {
#' router <- crew_router()
#' router$start()
#' router$daemons
#' router$terminate()
#' }
crew_router <- function(
  name = NULL,
  workers = 1L,
  host = NULL,
  port = NULL,
  seconds_interval = 0.25,
  seconds_timeout = 10
) {
  name <- as.character(name %|||% crew_random_name())
  workers <- as.integer(workers)
  host <- as.character(host %|||% getip::getip(type = "local"))
  port <- as.integer(port %|||% 0L)
  router <- crew_class_router$new(
    name = name,
    workers = workers,
    host = host,
    port = port,
    seconds_interval = seconds_interval,
    seconds_timeout = seconds_timeout
  )
  router$validate()
  router
}

#' @title Router class
#' @export
#' @family routers
#' @description `R6` class for `mirai` routers.
#' @details See [crew_router()].
#' @examples
#' if (identical(Sys.getenv("CREW_EXAMPLES"), "true")) {
#' router <- crew_router()
#' router$start()
#' router$daemons
#' router$terminate()
#' }
crew_class_router <- R6::R6Class(
  classname = "crew_class_router",
  cloneable = FALSE,
  public = list(
    #' @field name See [crew_router()].
    name = NULL,
    #' @field workers See [crew_router()].
    workers = NULL,
    #' @field host See [crew_router()].
    host = NULL,
    #' @field port See [crew_router()].
    port = NULL,
    #' @field seconds_interval See [crew_router()].
    seconds_interval = NULL,
    #' @field seconds_timeout See [crew_router()].
    seconds_timeout = NULL,
    #' @field started Whether the router is started.
    started = NULL,
    #' @field dispatcher Process ID of the `mirai` dispatcher
    dispatcher = NULL,
    #' @description `mirai` router constructor.
    #' @return An `R6` object with the router.
    #' @param name Argument passed from [crew_router()].
    #' @param workers Argument passed from [crew_router()].
    #' @param host Argument passed from [crew_router()].
    #' @param port Argument passed from [crew_router()].
    #' @param seconds_interval Argument passed from [crew_router()].
    #' @param seconds_timeout Argument passed from [crew_router()].
    #' @examples
    #' if (identical(Sys.getenv("CREW_EXAMPLES"), "true")) {
    #' router <- crew_router()
    #' router$start()
    #' router$daemons
    #' router$terminate()
    #' }
    initialize = function(
      name = NULL,
      workers = NULL,
      host = NULL,
      port = NULL,
      seconds_interval = NULL,
      seconds_timeout = NULL
    ) {
      self$name <- name
      self$workers <- workers
      self$host <- host
      self$port <- port
      self$seconds_interval <- seconds_interval
      self$seconds_timeout <- seconds_timeout
    },
    #' @description Validate the router.
    #' @return `NULL` (invisibly).
    validate = function() {
      crew_assert(
        self$name,
        is.character(.),
        length(.) == 1L,
        nzchar(.),
        !anyNA(.)
      )
      crew_assert(
        self$workers,
        is.integer(.),
        length(.) == 1L,
        !anyNA(.), . > 0L
      )
      crew_assert(
        self$host,
        is.character(.),
        length(.) == 1L,
        !anyNA(.),
        nzchar(.)
      )
      crew_assert(self$port, is.integer(.), length(.) == 1L, !anyNA(.))
      crew_assert(self$port, . >= 0L, . <= 65535L)
      fields <- c(
        "seconds_interval",
        "seconds_timeout"
      )
      for (field in fields) {
        crew_assert(
          self[[field]],
          is.numeric(.),
          length(.) == 1L,
          !is.na(.),
          . >= 0
        )
      }
      crew_assert(
        self$dispatcher %|||% 0L,
        is.numeric(.),
        length(.) == 1L,
        !is.na(.),
        . >= 0
      )
      crew_assert(self$seconds_timeout >= self$seconds_interval)
      invisible()
    },
    #' @description Start listening for workers on the available sockets.
    #' @return `NULL` (invisibly).
    start = function() {
      if (isTRUE(self$started)) {
        return(invisible())
      }
      args <- list(
        url = sprintf("ws://%s:%s", self$host, self$port),
        n = self$workers,
        dispatcher = TRUE,
        token = TRUE,
        .compute = self$name
      )
      do.call(what = mirai::daemons, args = args)
      # TODO: remove code that gets the dispatcher PID if the dispatcher
      # process becomes a C thread.
      # Begin dispatcher code.
      self$dispatcher <- environment(mirai::daemons)$..[[self$name]]$pid
      attr(rownames(self$daemons), "dispatcher_pid") <- self$dispatcher
      # End dispatcher code.
      self$started <- TRUE
      invisible()
    },
    #' @description Show an informative worker log.
    #' @return A `tibble` with information on the workers.
    log = function() {
      if (!isTRUE(self$started)) {
        return(NULL)
      }
      daemons <- daemons_info(name = self$name)
      sockets <- as.character(rownames(daemons))
      tibble::tibble(
        tasks_assigned = as.integer(daemons[, "assigned"]),
        tasks_complete = as.integer(daemons[, "complete"]),
        worker_connected = as.logical(daemons[, "online"] > 0L),
        worker_instances = as.integer(daemons[, "instance"]),
        worker_socket = sockets
      )
    },
    #' @description Stop the mirai client and disconnect from the
    #'   worker websockets.
    #' @return `NULL` (invisibly).
    terminate = function() {
      if (!isTRUE(self$started)) {
        return(invisible())
      }
      # TODO: if the dispatcher process becomes a C thread,
      # delete these superfluous checks on the dispatcher.
      # Begin dispatcher checks block 1/2.
      if (!is.null(self$dispatcher)) {
        handle <- ps::ps_handle(pid = self$dispatcher)
      }
      # End dispatcher checks block 1/2.
      mirai::daemons(n = 0L, .compute = self$name)
      self$started <- FALSE
      # Begin dispatcher checks block 2/2.
      if (is.null(self$dispatcher)) {
        return(invisible())
      }
      tryCatch(
        crew_retry(
          fun = ~!ps::ps_is_running(p = handle),
          seconds_interval = self$seconds_interval,
          seconds_timeout = self$seconds_timeout
        ),
        crew_expire = function(condition) NULL
      )
      if_any(
        ps::ps_is_running(p = handle),
        ps::ps_kill(p = handle),
        NULL
      )
      tryCatch(
        crew_retry(
          fun = ~!ps::ps_is_running(p = handle),
          seconds_interval = self$seconds_interval,
          seconds_timeout = self$seconds_timeout
        ),
        crew_expire = function(condition) NULL
      )
      # End dispatcher checks block 2/2.
      invisible()
    }
  )
)
