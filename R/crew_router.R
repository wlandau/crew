#' @title Create a router.
#' @export
#' @keywords internal
#' @family routers
#' @description Create an `R6` object to manage the `mirai` task scheduler
#'   client.
#' @param name Name of the router object. If `NULL`, a name is automatically
#'   generated.
#' @param workers Integer, maximum number of parallel workers to run.
#' @param port TCP port to listen for the workers. If `0`,
#'   then NNG automatically chooses an available ephemeral port.
#' @param router_timeout Number of seconds to time out waiting for the `mirai`
#'   client to (dis)connect.
#' @param router_wait Number of seconds to wait between iterations checking
#'   if the `mirai` client is (dis)connected.
#' @examples
#' if (identical(Sys.getenv("CREW_EXAMPLES"), "true")) {
#' router <- crew_router()
#' router$listen()
#' router$sockets() # "ws://xx.xx.xx:xxxxx/x"
#' router$terminate()
#' }
crew_router <- function(
  name = NULL,
  workers = 1L,
  port = 0L,
  router_timeout = 5,
  router_wait = 0.1
) {
  true(workers, is.numeric(.), length(.) == 1L, . > 0L, !anyNA(.))
  name <- as.character(name %|||% random_name())
  workers <- as.integer(workers)
  port <- as.integer(port)
  true(name, is.character(.), length(.) == 1L, nzchar(.), !anyNA(.))
  true(workers, is.integer(.), length(.) == 1L, !anyNA(.), . > 0L)
  true(port, is.integer(.), length(.) == 1L, !anyNA(.))
  true(port, . >= 0L, . <= 65535L)
  true(router_timeout, is.numeric(.), length(.) == 1L, !is.na(.), . >= 0)
  true(router_wait, is.numeric(.), length(.) == 1L, !is.na(.), . >= 0)
  true(router_timeout >= router_wait)
  router <- crew_class_router$new(
    name = name,
    workers = workers,
    port = port,
    router_timeout = router_timeout,
    router_wait = router_wait
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
#' router$sockets() # "ws://xx.xx.xx:xxxxx/x"
#' router$terminate()
#' }
crew_class_router <- R6::R6Class(
  classname = "crew_class_router",
  cloneable = FALSE,
  public = list(
    #' @field name Name of the router.
    name = NULL,
    #' @field workers Number of workers.
    workers = NULL,
    #' @field port Local TCP port.
    port = NULL,
    #' @field router_timeout Timeout in seconds for checking the `mirai`
    #'   client connection.
    router_timeout = NULL,
    #' @field router_wait Polling interval in seconds for checking the
    #'   `mirai` client connection.
    router_wait = NULL,
    #' @field sockets Character vector of sockets listening for
    #'   completed tasks.
    sockets = NULL,
    #' @description `mirai` router constructor.
    #' @return An `R6` object with the router.
    #' @param name Argument passed from [crew_router()].
    #' @param workers Argument passed from [crew_router()].
    #' @param port Argument passed from [crew_router()].
    #' @param router_timeout Argument passed from [crew_router()].
    #' @param router_wait Argument passed from [crew_router()].
    #' @examples
    #' if (identical(Sys.getenv("CREW_EXAMPLES"), "true")) {
    #' router <- crew_router()
    #' router$listen()
    #' router$sockets() # "ws://xx.xx.xx:xxxxx/x"
    #' router$terminate()
    #' }
    initialize = function(
      name = NULL,
      workers = NULL,
      port = NULL,
      router_timeout = NULL,
      router_wait = NULL
    ) {
      self$name <- name
      self$workers <- workers
      self$port <- port
      self$router_timeout <- router_timeout
      self$router_wait <- router_wait
    },
    #' @description Validate the router.
    #' @return `NULL` (invisibly).
    validate = function() {
      true(self$name, is.character(.), length(.) == 1L, nzchar(.), !anyNA(.))
      true(self$workers, is.integer(.), length(.) == 1L, !anyNA(.), . > 0L)
      true(self$port, is.integer(.), length(.) == 1L, !anyNA(.))
      true(self$port, . >= 0L, . <= 65535L)
      true(
        self$router_timeout,
        is.numeric(.),
        length(.) == 1L,
        !is.na(.), . >= 0
      )
      true(self$router_wait, is.numeric(.), length(.) == 1L, !is.na(.), . >= 0)
      true(self$router_timeout >= self$router_wait)
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
      (length(out) == 1L) &&
        !anyNA(out) &&
        is.numeric(out) &&
        out > 0L
    },
    #' @description Start listening for workers on the available sockets.
    #' @return `NULL` (invisibly).
    listen = function() {
      if (isFALSE(self$listening())) {
        socket <- sprintf("ws://%s:%s", local_ip(), self$port)
        mirai::daemons(
          url = socket,
          n = self$workers,
          active = TRUE,
          .compute = self$name
        )
        crew_wait(
          fun = ~isTRUE(self$listening()),
          timeout = self$router_timeout,
          wait = self$router_wait,
          message = "mirai client cannot connect."
        )
        daemons <- mirai::daemons(.compute = self$name)$daemons
        self$sockets <- if_any(
          anyNA(daemons),
          character(0),
          rownames(daemons)
        )
      }
      invisible()
    },
    #' @description Stop the mirai client and disconnect from the
    #'   worker websockets.
    #' @return `NULL` (invisibly).
    terminate = function() {
      if (isTRUE(self$listening())) {
        try(mirai::daemons(n = 0L, .compute = self$name), silent = TRUE)
        try(
          crew_wait(
            fun = ~isFALSE(self$listening()),
            timeout = self$router_timeout,
            wait = self$router_wait,
            message = "mirai client could not terminate."
          ),
          silent = TRUE
        )
        self$sockets <- NULL
      }
      invisible()
    }
  )
)
