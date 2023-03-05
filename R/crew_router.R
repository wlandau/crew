#' @title Create a router.
#' @export
#' @keywords internal
#' @family routers
#' @description Create an `R6` object to manage the `mirai` task scheduler
#'   client.
#' @param name Name of the router object. If `NULL`, a name is automatically
#'   generated.
#' @param workers Integer, maximum number of parallel workers to run.
#' @param host IP address of the client process that the workers can dial
#'   into inside the local network.
#'   If a character string, the router uses the specified IP address.
#'   If `NULL`, the IP address defaults to `getip::getip(type = "local")`.
#' @param port TCP port to listen for the workers. If `NULL`,
#'   then an available port is supplied through `parallelly::freePort()`.
#' @param router_timeout Number of seconds to time out waiting for the `mirai`
#'   client to (dis)connect.
#' @param router_wait Number of seconds to wait between iterations checking
#'   if the `mirai` client is (dis)connected.
#' @examples
#' if (identical(Sys.getenv("CREW_EXAMPLES"), "true")) {
#' router <- crew_router()
#' router$sockets_listening() # character(0)
#' router$connect()
#' router$sockets_listening() # "ws://xx.xx.xx:xxxxx/x"
#' router$disconnect()
#' router$sockets_listening() # character(0)
#' }
crew_router <- function(
  name = NULL,
  workers = 1L,
  host = NULL,
  port = NULL,
  router_timeout = 5,
  router_wait = 0.1
) {
  true(workers, is.numeric(.), length(.) == 1L, . > 0L, !anyNA(.))
  name <- as.character(name %|||% random_name())
  host <- as.character(host %|||% local_ipv4())
  port <- as.integer(port %|||% free_port())
  true(name, is.character(.), length(.) == 1L, nzchar(.), !anyNA(.))
  true(host, is.character(.), length(.) == 1L, nzchar(.), !anyNA(.))
  true(port, is.integer(.), length(.) == 1L, !anyNA(.))
  true(port, . >= 0L, . <= 65535L)
  true(router_timeout, is.numeric(.), length(.) == 1L, !is.na(.), . >= 0)
  true(router_wait, is.numeric(.), length(.) == 1L, !is.na(.), . >= 0)
  true(router_timeout >= router_wait)
  sockets <- web_sockets(host = host, port = port, n = workers)
  router <- crew_class_router$new(
    name = name,
    sockets = sockets,
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
#' router$sockets_listening() # character(0)
#' router$connect()
#' router$sockets_listening() # "ws://xx.xx.xx:xxxxx/x"
#' router$disconnect()
#' router$sockets_listening() # character(0)
#' }
crew_class_router <- R6::R6Class(
  classname = "crew_class_router",
  cloneable = FALSE,
  public = list(
    #' @field name Name of the router.
    name = NULL,
    #' @field sockets Websockets to listen for workers.
    sockets = NULL,
    #' @field router_timeout Timeout in seconds for checking the `mirai`
    #'   client connection.
    router_timeout = NULL,
    #' @field router_wait Polling interval in seconds for checking the
    #'   `mirai` client connection.
    router_wait = NULL,
    #' @description `mirai` router constructor.
    #' @return An `R6` object with the router.
    #' @param name Argument passed from [crew_router()].
    #' @param sockets Argument passed from [crew_router()].
    #' @param router_timeout Argument passed from [crew_router()].
    #' @param router_wait Argument passed from [crew_router()].
    #' @examples
    #' if (identical(Sys.getenv("CREW_EXAMPLES"), "true")) {
    #' router <- crew_router()
    #' router$sockets_listening() # character(0)
    #' router$connect()
    #' router$sockets_listening() # "ws://xx.xx.xx:xxxxx/x"
    #' router$disconnect()
    #' router$sockets_listening() # character(0)
    #' }
    initialize = function(
      name = NULL,
      sockets = NULL,
      router_timeout = NULL,
      router_wait = NULL
    ) {
      self$name <- name
      self$sockets <- sockets
      self$router_timeout <- router_timeout
      self$router_wait <- router_wait
    },
    #' @description Validate the router.
    #' @return `NULL` (invisibly).
    validate = function() {
      true(self$name, is.character(.), length(.) == 1L, nzchar(.), !anyNA(.))
      true(self$sockets, is.character(.), nzchar(.), !anyNA(.))
      true(all(grepl(pattern = "^ws\\:\\/\\/", x = self$sockets)))
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
    #' @description Get occupied worker sockets.
    #' @return Character vector of websockets sockets where workers
    #'   are currently dialed in.
    occupied = function() {
      nodes <- mirai::daemons(.compute = self$name)$nodes
      as.character(names(nodes)[nodes == 1L])
    },
    #' @description Get worker sockets that are unoccupied and
    #'   available for workers to dial in.
    #' @return Character string with available websockets.
    unoccupied = function() {
      nodes <- mirai::daemons(.compute = self$name)$nodes
      as.character(names(nodes)[nodes == 0L])
    },
    #' @description Check if the router is connected.
    #' @details This method may stall and time out if there are
    #'   tasks in the queue. Methods `connect()` and `disconnect()`
    #'   call `connected()` to manage the connection before
    #'   and after the entire workload, respectively.
    #' @return `TRUE` if successfully listening for dialed-in workers,
    #'   `FALSE` otherwise.
    connected = function() {
      out <- mirai::daemons(.compute = self$name)$connections
      (length(out) == 1L) && !anyNA(out) && is.numeric(out) && out > 0L
    },
    #' @description Start listening for workers on the available sockets.
    #' @return `NULL` (invisibly).
    connect = function() {
      if (isFALSE(self$connected())) {
        args <- list(
          value = self$sockets,
          nodes = length(self$sockets),
          .compute = self$name
        )
        do.call(what = mirai::daemons, args = args)
        crew_wait(
          fun = ~isTRUE(self$connected()),
          timeout = self$router_timeout,
          wait = self$router_wait,
          message = "mirai client cannot connect."
        )
      }
      invisible()
    },
    #' @description Disconnect the router.
    #' @return `NULL` (invisibly).
    disconnect = function() {
      if (isTRUE(self$connected())) {
        try(mirai::daemons(value = 0L, .compute = self$name), silent = TRUE)
        try(
          crew_wait(
            fun = ~isFALSE(self$connected()),
            timeout = self$router_timeout,
            wait = self$router_wait,
            message = "mirai client cannot disconnect."
          ),
          silent = TRUE
        )
      }
      invisible()
    }
  )
)
