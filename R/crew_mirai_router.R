#' @title Create a `mirai` router.
#' @export
#' @family routers
#' @description Create an `R6` object to route tasks to `mirai` workers
#'   on the local network.
#' @param name Name of the `mirai` router.
#'   Defaults to a string from `ids::proquint()`.
#' @param host IP address the router uses to connect to the workers.
#'   If a character string, the router uses the specified IP address.
#'   If `NULL`, the IP address defaults to `getip::getip(type = "local")`.
#' @param port TCP port the router uses to connect to the workers.
#'   If an integer or character string, the router uses the specified port.
#'   If `NULL`, the port defaults to an available port in the ephemeral
#'   port range (49152 through 65355).
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
  port = NULL
) {
  router <- crew_class_mirai_router$new(
    name = as.character(name %|||% random_name()),
    host = as.character(host %|||% local_ipv4()),
    port = as.integer(port %|||% random_port())
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
    #' @field host IP address the router uses to connect to the workers.
    host = NULL,
    #' @field port TCP port the router uses to connect to the workers.
    port = NULL,
    #' @description `mirai` router constructor.
    #' @return An `R6` object with the router.
    #' @param name Name of the router.
    #' @param host Host of the router. See [crew_mirai_router()].
    #' @param port TCP port of the router. See [crew_mirai_router()].
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
      port = NULL
    ) {
      self$name <- name
      self$host <- host
      self$port <- port
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
      true(self$port, is.integer(.), length(.) == 1L, . > 0L, !anyNA(.))
      true(self$port, . >= 0L, . <= 65535L)
      invisible()
    },
    #' @description Get the TCP socket of the router.
    #' @return Character string with the TCP socket of the router.
    socket = function() {
      tcp_socket(host = self$host, port = self$port)
    },
    #' @description Check if the router is connected.
    #' @return `TRUE` if connected and `FALSE` otherwise.
    connected = function() {
      identical(mirai::daemons(.compute = self$name)$daemons, "remote")
    },
    #' @description Return the number of workers connected to the router.
    #' @return Nonnegative integer, number of connected workers.
    connections = function() {
      mirai::daemons(.compute = self$name)$connections
    },
    #' @description Connect the router to the host and port.
    #' @return `NULL` (invisibly).
    connect = function() {
      if (!self$connected()) {
        mirai::daemons(
          value = tcp_socket(host = self$host, port = self$port),
          .compute = self$name
        )
      }
      invisible()
    },
    #' @description Disconnect the router from the host and port.
    #' @return `NULL` (invisibly).
    disconnect = function() {
      mirai::daemons(value = 0L, .compute = self$name)
      invisible()
    }
  )
)
