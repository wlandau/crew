#' @title Create a `mirai` router.
#' @export
#' @family routers
#' @description Create an `R6` object to route tasks to `mirai` workers
#'   on the local network.
#' @param name Name of the `mirai` router.
#'   Defaults to a string from `ids::proquint()`.
#' @param host IP address of the client process that the workers can dial
#'   into inside the local network.
#'   If a character string, the router uses the specified IP address.
#'   If `NULL`, the IP address defaults to `getip::getip(type = "local")`.
#' @param ports Integer vector of TCP port numbers, one for each port
#'   on the client where a worker (`mirai` server) may dial in and accept tasks.
#'   For random available ports, you can use `parallelly::freePort()`.
#'   Defaults to a single port from `parallelly::freePort()`
#'   in the ephemeral range (49152 to 65535).
#' @examples
#' if (identical(Sys.getenv("CREW_EXAMPLES"), "true")) {
#' router <- crew_mirai_router()
#' router$validate()
#' router$connect()
#' # Now launch workers with a mirai launcher.
#' router$disconnect()
#' }
crew_mirai_router <- function(
  name = NULL,
  host = NULL,
  ports = NULL
) {
  router <- crew_class_mirai_router$new(
    name = as.character(name %|||% random_name()),
    host = as.character(host %|||% local_ipv4()),
    ports = as.integer(ports %|||% random_port())
  )
  router$validate()
  router
}

#' @title `mirai` router class
#' @export
#' @family routers
#' @description `R6` class for `mirai` routers.
#' @details See [crew_mirai_router()].
crew_class_mirai_router <- R6::R6Class(
  classname = "crew_class_mirai_router",
  cloneable = FALSE,
  public = list(
    #' @field name Name of the router.
    name = NULL,
    #' @field host Client IP address. See [crew_mirai_router()] for details.
    host = NULL,
    #' @field ports TCP ports. See [crew_mirai_router()] for details.
    ports = NULL,
    #' @description `mirai` router constructor.
    #' @return An `R6` object with the router.
    #' @param name Name of the router object.
    #' @param host Client IP address. See [crew_mirai_router()] for details.
    #' @param ports TCP ports. See [crew_mirai_router()] for details.
    #' @examples
    #' if (identical(Sys.getenv("CREW_EXAMPLES"), "true")) {
    #' router <- crew_mirai_router()
    #' router$validate()
    #' router$connect()
    #' # Now launch workers with a mirai launcher.
    #' router$disconnect()
    #' }
    initialize = function(
      name = NULL,
      host = NULL,
      ports = NULL
    ) {
      self$name <- name
      self$host <- host
      self$ports <- ports
      invisible()
    },
    #' @description Disconnect at garbage collection time.
    #' @return `NULL` (invisibly).
    finalize = function() {
      self$disconnect()
    },
    #' @description Validate the router.
    #' @return `NULL` (invisibly).
    validate = function() {
      true(self$name, is.character(.), length(.) == 1L, nzchar(.), !anyNA(.))
      true(self$host, is.character(.), length(.) == 1L, nzchar(.), !anyNA(.))
      true(self$ports, is.integer(.), length(.) > 0L, . > 0L, !anyNA(.))
      true(self$ports, . >= 0L, . <= 65535L)
      invisible()
    },
    #' @description Get the TCP socket of the router.
    #' @return Character string with the TCP socket of the router.
    sockets = function() {
      tcp_sockets(host = self$host, ports = self$ports)
    },
    #' @description Check if the router is connected.
    #' @return `TRUE` if connected and `FALSE` otherwise.
    connected = function() {
      identical(mirai::daemons(.compute = self$name)$daemons, "remote")
    },
    #' @description Show worker connections.
    #' @return Character vector of TCP sockets where the client is
    #'   listening to currently dialed-in running workers.
    connections = function() {
      out <- mirai::daemons(.compute = self$name)$nodes
      as.character(names(out)[out > 0L])
    },
    #' @description Start listening for workers on the available sockets.
    #' @return `NULL` (invisibly).
    connect = function() {
      if (!self$connected()) {
        tcp <- self$sockets()
        mirai::daemons(value = tcp, nodes = length(tcp), .compute = self$name)
      }
      invisible()
    },
    #' @description Disconnect the router.
    #' @return `NULL` (invisibly).
    disconnect = function() {
      try(mirai::daemons(value = 0L, .compute = self$name), silent = TRUE)
      invisible()
    }
  )
)
