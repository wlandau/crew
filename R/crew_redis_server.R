#' @title Redis server
#' @export
#' @family redis
#' @description Create a handle to configure, run, and manage
#'   an instance of Redis server.
#' @return An `R6` object with methods to configure, run,
#'   and manage an instance of Redis server.
#' @param binary Nonempty non-missing character of length 1,
#'   path to the Redis server executable.
#'   The path must point to an existing file.
#'   If the `binary` argument is `NULL`,
#'   then the value defaults to `Sys.getenv("CREW_REDIS_SERVER_BINARY")`
#'   if this environment variable is set. Otherwise, the default becomes
#'   `unname(Sys.which("redis-server"))`.
#'   Visit <https://redis.io/> to learn how to install Redis server.
#' @param conf Non-missing character of length 1,
#'   path to the Redis server configuration file.
#'   The default value of the `conf` argument
#'   is `Sys.getenv("CREW_REDIS_SERVER_CONF")` if this environment
#'   variable is set. Otherwise, the default becomes
#'   `""` to use the system default configuration file.
#'   Visit <https://redis.io/docs/management/config/>
#'   to learn how to configure Redis.
#' @param host Nonempty non-missing character of length 1,
#'   IP address of the Redis server.
#'   The default value is `Sys.getenv("CREW_REDIS_SERVER_HOST")`,
#'   if this environment variable is set. Otherwise, the default value
#'   is the IPv4 loopback address `"127.0.0.1"`.
#' @param port Nonempty non-missing character of length 1,
#'   TCP port of the Redis server.
#'   The default value is `Sys.getenv("CREW_REDIS_SERVER_PORT")`
#'   if this environment variable is set. Otherwise, the default is
#'   a random ephemeral port.
#' @param password Non-missing character of length 1
#'   with at most 64 characters,
#'   password to the Redis server (default user).
#'   The default value is `Sys.getenv("CREW_REDIS_SERVER_PASSWORD")`
#'   if this environment variable is set. Otherwise, the default is a
#'   a random cryptographic 256-bit 64-character password.
#'   Visit <https://redis.io/docs/management/security/> to learn about
#'   Redis security.
#' @param start_timeout Positive numeric of length 1, number of seconds
#'   to wait for the Redis server to start up and begin accepting clients.
#' @param start_wait Positive numeric of length 1, polling interval
#'   (seconds) while waiting for the Redis server to start up and
#'   begin accepting clients.
#' @examples
#' if (identical(Sys.getenv("CREW_EXAMPLES"), "true")) {
#' server <- crew_redis_server()
#' server$validate()
#' server$test() # TRUE
#' server$start()
#' server$ping() # [Redis: PONG]
#' server$stop()
#' }
crew_redis_server <- function(
  binary = NULL,
  conf = NULL,
  host = NULL,
  port = NULL,
  password = NULL,
  start_timeout = 5,
  start_wait = 0.25
) {
  out <- redis_server$new(
    binary = binary %|||% redis_server_default_binary(),
    conf = conf %|||% redis_server_default_conf(),
    host = host %|||% redis_server_default_host(),
    port = port %|||% redis_server_default_port(),
    password = password %|||% redis_server_default_password(),
    start_timeout = start_timeout,
    start_wait = start_wait
  )
  out$validate()
  out
}

redis_server <- R6::R6Class(
  classname = "redis_server",
  public = list(
    binary = NULL,
    conf = NULL,
    host = NULL,
    port = NULL,
    password = NULL,
    process = NULL,
    start_timeout = NULL,
    start_wait = NULL,
    initialize = function(
      binary = NULL,
      conf = NULL,
      host = NULL,
      port = NULL,
      password = NULL,
      start_timeout = NULL,
      start_wait = NULL
    ) {
      self$binary <- binary
      self$conf <- conf
      self$host <- host
      self$port <- port
      self$password <- password
      self$start_timeout <- start_timeout
      self$start_wait <- start_wait
    },
    finalize = function() {
      self$stop()
    },
    serialize = function() {
      redis_server_serialize(self)
    },
    client = function() {
      redis_server_client(self)
    },
    start = function() {
      redis_server_start(self)
    },
    stop = function() {
      redis_server_stop(self)
    },
    alive = function() {
      redis_server_alive(self)
    },
    ready = function() {
      redis_server_ready(self)
    },
    ping = function() {
      redis_server_ping(self)
    },
    test = function() {
      redis_server_test(self)
    },
    validate = function() {
      redis_server_validate(self)
    },
    configured = function() {
      redis_server_configured(self)
    }
  )
)

redis_server_default_binary <- function() {
  Sys.getenv(
    "CREW_REDIS_SERVER_BINARY",
    unset = unname(Sys.which("redis-server"))
  )
}

redis_server_default_conf <- function() {
  out <- Sys.getenv("CREW_REDIS_SERVER_CONF", unset = "")
  if_any(nzchar(out), out, NULL)
}

redis_server_default_host <- function() {
  Sys.getenv("CREW_REDIS_SERVER_HOST", unset = "127.0.0.1")
}

redis_server_default_port <- function() {
  Sys.getenv("CREW_REDIS_SERVER_PORT", unset = redis_server_random_port())
}

redis_server_random_port <- function(lower = 49152L, upper = 65535L) {
  ports <- seq.int(from = lower, to = upper, by = 1L)
  as.character(sample(ports, size = 1L))
}

redis_server_default_password <- function() {
  Sys.getenv(
    "CREW_REDIS_SERVER_PASSWORD",
    unset = redis_server_random_password()
  )
}

redis_server_random_password <- function() {
  digest::digest(
    object = openssl::rand_bytes(n = 256),
    algo = "sha256",
    serialize = FALSE
  )
}

redis_server_serialize <- function(self) {
  fields <- c("host", "port", "password")
  values <- map_chr(fields, ~self[[.x]])
  pairs <- paste(fields, values, sep = "=")
  paste(pairs, collapse = "&")
}

redis_server_start <- function(self) {
  if (!self$alive()) {
    self$process <- redis_server_process(
      binary = self$binary,
      conf = self$conf,
      host = self$host,
      port = self$port,
      password = self$password
    )
  }
  crew_wait(
    fun = self$alive,
    timeout = self$start_timeout,
    wait = self$start_wait,
    message = paste(
      c(
        "Redis server could not start.\n",
        self$process$read_all_output(),
        self$process$read_all_error()
      ),
      collapse = ""
    )
  )
  crew_wait(
    fun = self$ready,
    timeout = self$start_timeout,
    wait = self$start_wait,
    message = "Redis server could not initialize or receive connections."
  )
  invisible()
}

redis_server_process <- function(
  binary,
  conf,
  host,
  port,
  password
) {
  args <- c(
    conf,
    "--bind",
    host,
    "--port",
    port,
    "--requirepass",
    password
  )
  processx::process$new(
    command = binary,
    args = args,
    stdout = "|",
    stderr = "|",
    cleanup = TRUE,
    supervise = TRUE
  )
}

redis_server_stop <- function(self) {
  if (!is.null(self$process)) {
    self$process$kill()
  }
  invisible()
}

redis_server_alive <- function(self) {
  (!is.null(self$process)) && (self$process$is_alive())
}

redis_server_client <- function(self) {
  crew_true(
    self$alive(),
    message = "Redis server must start before accepting clients."
  )
  redux::hiredis(
    host = self$host,
    port = self$port,
    password = self$password
  )
}

redis_server_ping <- function(self) {
  client <- self$client()
  client$PING()
}

redis_server_ready <- function(self) {
  tryCatch(
    identical(as.character(self$ping()), "PONG"),
    error = crew_condition_false
  )
}

redis_server_test <- function(self) {
  crew_true(!self$alive(), message = "Redis server already running.")
  on.exit(self$stop())
  try(self$start(), silent = TRUE)
  self$ready()
}

redis_server_validate <- function(self) {
  crew_true(
    self$conf %|||% "redis.conf",
    is.character(.),
    length(.) == 1L,
    !anyNA(.),
    message = "conf must be NULL, or a non-missing character of length 1."
  )
  for (field in c("binary", "host", "port", "password")) {
    message <- paste(
      field,
      "must be a non-missing non-empty character of length 1."
    )
    crew_true(
      self[[field]],
      is.character(.),
      length(.) == 1L,
      !anyNA(.),
      nzchar(.),
      message = message
    )
  }
  crew_true(
    as.integer(self$port),
    !anyNA(.),
    length(.) == 1L,
    . >= 0L,
    . <= 65535L,
    message = "invalid TCP port"
  )
  crew_true(
    nchar(self$password) <= 64L,
    message = "password must be at most 64 characters."
  )
  message <- sprintf("Redis binary %s not found.", self$binary)
  crew_true(file.exists(self$binary), message = message)
  if (!is.null(self$conf)) {
    message <- sprintf("Redis configuration file %s not found.", self$conf)
    crew_true(file.exists(self$conf), message = message)
  }
  invisible()
}

redis_server_configured <- function(self) {
  tryCatch(
    redis_server_validate(self) %|||% TRUE,
    error = crew_condition_false
  )
}
