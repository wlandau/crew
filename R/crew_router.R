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
#' @param port TCP port to listen for the workers. If `0`,
#'   then NNG automatically chooses an available ephemeral port.
#' @param seconds_interval Number of seconds between
#'   polling intervals waiting for certain internal
#'   synchronous operations to complete.
#' @param seconds_timeout Number of seconds until timing
#'   out while waiting for certain synchronous operations to complete.
#' @param seconds_exit Number of seconds to wait for NNG websockets
#'   to finish sending large data (in case an exit signal is received).
#' @param seconds_poll_high High polling interval in seconds for the
#'   `mirai` active queue. See the `pollfreqh` argument of
#'   `mirai::dispatcher()`.
#' @param seconds_poll_low Low polling interval in seconds for the `mirai`
#'   active queue. See the `pollfreql` argument of
#'   `mirai::dispatcher()`.
#' @param async_dial Logical, whether the `mirai` workers should dial in
#'   asynchronously. See the `asyncdial` argument of `mirai::server()`.
#' @examples
#' if (identical(Sys.getenv("CREW_EXAMPLES"), "true")) {
#' router <- crew_router()
#' router$listen()
#' router$daemons
#' router$terminate()
#' }
crew_router <- function(
  name = NULL,
  workers = 1L,
  host = NULL,
  port = 0L,
  seconds_interval = 0.001,
  seconds_timeout = 5,
  seconds_exit = 0.1,
  seconds_poll_high = 0.001,
  seconds_poll_low = 0.01,
  async_dial = TRUE
) {
  name <- as.character(name %|||% random_name())
  workers <- as.integer(workers)
  host <- as.character(host %|||% local_ip())
  port <- as.integer(port)
  router <- crew_class_router$new(
    name = name,
    workers = workers,
    host = host,
    port = port,
    seconds_interval = seconds_interval,
    seconds_timeout = seconds_timeout,
    seconds_exit = seconds_exit,
    seconds_poll_high = seconds_poll_high,
    seconds_poll_low = seconds_poll_low,
    async_dial = async_dial
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
#' router$listen()
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
    #' @field seconds_exit See [crew_router()].
    seconds_exit = NULL,
    #' @field seconds_poll_high See [crew_router()].
    seconds_poll_high = NULL,
    #' @field seconds_poll_low See [crew_router()].
    seconds_poll_low = NULL,
    #' @field async_dial See [crew_router()].
    async_dial = NULL,
    #' @field dispatcher Process ID of the `mirai` dispatcher
    dispatcher = NULL,
    #' @field daemons Data frame of information from `mirai::daemons()`.
    daemons = NULL,
    #' @description `mirai` router constructor.
    #' @return An `R6` object with the router.
    #' @param name Argument passed from [crew_router()].
    #' @param workers Argument passed from [crew_router()].
    #' @param host Argument passed from [crew_router()].
    #' @param port Argument passed from [crew_router()].
    #' @param seconds_interval Argument passed from [crew_router()].
    #' @param seconds_timeout Argument passed from [crew_router()].
    #' @param seconds_exit Argument passed from [crew_router()].
    #' @param seconds_poll_high Argument passed from [crew_router()].
    #' @param seconds_poll_low Argument passed from [crew_router()].
    #' @param async_dial Argument passed from [crew_router()].
    #' @examples
    #' if (identical(Sys.getenv("CREW_EXAMPLES"), "true")) {
    #' router <- crew_router()
    #' router$listen()
    #' router$daemons
    #' router$terminate()
    #' }
    initialize = function(
      name = NULL,
      workers = NULL,
      host = NULL,
      port = NULL,
      seconds_interval = NULL,
      seconds_timeout = NULL,
      seconds_exit = NULL,
      seconds_poll_high = NULL,
      seconds_poll_low = NULL,
      async_dial = NULL
    ) {
      self$name <- name
      self$workers <- workers
      self$host <- host
      self$port <- port
      self$seconds_interval <- seconds_interval
      self$seconds_timeout <- seconds_timeout
      self$seconds_exit <- seconds_exit
      self$seconds_poll_high <- seconds_poll_high
      self$seconds_poll_low <- seconds_poll_low
      self$async_dial <- async_dial
    },
    #' @description Validate the router.
    #' @return `NULL` (invisibly).
    validate = function() {
      true(self$name, is.character(.), length(.) == 1L, nzchar(.), !anyNA(.))
      true(self$workers, is.integer(.), length(.) == 1L, !anyNA(.), . > 0L)
      true(self$host, is.character(.), length(.) == 1L, !anyNA(.), nzchar(.))
      true(self$port, is.integer(.), length(.) == 1L, !anyNA(.))
      true(self$port, . >= 0L, . <= 65535L)
      fields <- c(
        "seconds_interval",
        "seconds_timeout",
        "seconds_exit",
        "seconds_poll_high",
        "seconds_poll_low"
      )
      for (field in fields) {
        true(
          self[[field]],
          is.numeric(.),
          length(.) == 1L,
          !is.na(.),
          . >= 0
        )
      }
      true(
        self$dispatcher %|||% 0L,
        is.numeric(.),
        length(.) == 1L,
        !is.na(.),
        . >= 0
      )
      true(self$seconds_timeout >= self$seconds_interval)
      true(self$async_dial, isTRUE(.) || isFALSE(.))
      true(self$daemons, is.null(.) || is.data.frame(.))
      invisible()
    },
    #' @description Check if the `mirai` client is listening
    #'   to worker websockets.
    #' @details This method may stall and time out if there are
    #'   tasks in the queue. Methods `listen()` and `terminate()`
    #'   call `listening()` to manage the connection before
    #'   and after the entire workload, respectively.
    #' @return `TRUE` if successfully listening for dialed-in workers,
    #'   `FALSE` otherwise.
    listening = function() {
      out <- mirai::daemons(.compute = self$name)$connections
      dispatcher <- attr(dimnames(daemons)[[1]], "dispatcher_pid")
      handle <- ps::ps_handle(pid = dispatcher)
      ps::ps_is_running(p = handle) &&
        (length(out) == 1L) &&
        !anyNA(out) &&
        is.numeric(out) &&
        out > 0L
    },
    #' @description Start listening for workers on the available sockets.
    #' @return `NULL` (invisibly).
    listen = function() {
      if (isFALSE(self$listening())) {
        socket <- sprintf("ws://%s:%s", self$host, self$port)
        args <- list(
          url = socket,
          n = self$workers,
          dispatcher = TRUE,
          asyncdial = self$async_dial,
          exitlinger = self$seconds_exit * 1000,
          pollfreqh = self$seconds_poll_high * 1000,
          pollfreql = self$seconds_poll_low * 1000,
          .compute = self$name
        )
        do.call(what = mirai::daemons, args = args)
        crew_wait(
          fun = ~isTRUE(self$listening()),
          seconds_interval = self$seconds_interval,
          seconds_timeout = self$seconds_timeout,
          message = "mirai client cannot connect."
        )
        self$poll()
      }
      invisible()
    },
    #' @description Update the `daemons` field with
    #'   information on the `mirai` daemons.
    #' @details Call `mirai::daemons()` to get information about the workers.
    #'   If the workers cannot be reached or the router has not started,
    #'   then do not modify the `daemons` field. Otherwise, if the workers
    #'   are reachable, populate the `daemons` field with a data frame
    #'   of high-level worker-specific statistics.
    #' @return `NULL` (invisibly).
    poll = function() {
      daemons <- mirai::daemons(.compute = self$name)$daemons
      if (is.null(dim(daemons))) {
        if (!is.null(dim(self$daemons))) {
          self$daemons$worker_connected <- rep(NA, nrow(self$daemons))
          self$daemons$worker_busy <- rep(NA, nrow(self$daemons))
        }
        return(invisible())
      }
      self$dispatcher <- attr(dimnames(daemons)[[1]], "dispatcher_pid")
      self$daemons <- tibble::tibble(
        worker_socket = as.character(rownames(daemons)),
        worker_connected = as.logical(
          daemons[, "status_online", drop = TRUE] > 0L
        ),
        worker_busy = as.logical(
          daemons[, "status_busy", drop = TRUE] > 0L
        ),
        worker_instances = as.integer(daemons[, "instance #", drop = TRUE]),
        tasks_assigned = as.integer(daemons[, "tasks_assigned", drop = TRUE]),
        tasks_complete = as.integer(daemons[, "tasks_complete", drop = TRUE])
      )
      invisible()
    },
    #' @description Stop the mirai client and disconnect from the
    #'   worker websockets.
    #' @return `NULL` (invisibly).
    terminate = function() {
      if (isTRUE(self$listening())) {
        self$poll()
        daemons <- mirai::daemons(.compute = self$name)$daemons
        dispatcher <- attr(dimnames(daemons)[[1]], "dispatcher_pid")
        handle <- ps::ps_handle(pid = dispatcher)
        mirai::daemons(n = 0L, .compute = self$name)
        crew_wait(
          fun = ~isFALSE(self$listening()),
          seconds_interval = self$seconds_interval,
          seconds_timeout = self$seconds_timeout,
          message = "mirai client could not terminate."
        )
        tryCatch(
          crew_wait(
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
          crew_wait(
            fun = ~!ps::ps_is_running(p = handle),
            seconds_interval = self$seconds_interval,
            seconds_timeout = self$seconds_timeout
          ),
          crew_expire = function(condition) NULL
        )
        self$poll()
      }
      invisible()
    }
  )
)
