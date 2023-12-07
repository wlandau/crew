#' @title Configure TLS.
#' @export
#' @family tls
#' @description Create an `R6` object with transport layer security (TLS)
#'   configuration for `crew`.
#' @details [crew_tls()] objects are input to the `tls` argument of
#'   [crew_client()], [crew_controller_local()], etc.
#'   See <https://wlandau.github.io/crew/articles/risks.html> for details.
#' @return An `R6` object with TLS configuration settings and methods.
#' @param mode Character of length 1. Must be one of the following:
#'   * `"none"`: disable TLS configuration.
#'   * `"automatic"`: let `mirai` create a one-time key pair with a
#'     self-signed certificate.
#'   * `"custom"`: manually supply a private key pair, an optional
#'     password for the private key, a certificate,
#'     an optional revocation list.
#' @param key If `mode` is `"none"` or `"automatic"`, then `key` is `NULL`.
#'   If `mode` is `"custom"`, then `key` is a character of length 1
#'   with the file path to the private key file.
#' @param password If `mode` is `"none"` or `"automatic"`,
#'   then `password` is `NULL`.
#'   If `mode` is `"custom"` and the private key is not encrypted, then
#'   `password` is still `NULL`.
#'   If `mode` is `"custom"` and the private key is encrypted,
#'   then `password` is a character of length 1 the the password of the private
#'   key. In this case, DO NOT SAVE THE PASSWORD IN YOUR R CODE FILES.
#'   See the `keyring` R package for solutions.
#' @param certificates If `mode` is `"none"` or `"automatic"`,
#'   then `certificates` is `NULL`.
#'   If `mode` is `"custom"`, then `certificates` is a character vector
#'   of file paths to certificate files (signed public keys).
#'   If the certificate is self-signed or if it is
#'   directly signed by a certificate authority (CA),
#'   then only the certificate of the CA is needed. But if you have a whole
#'   certificate chain which begins at your own certificate and ends with the
#'   CA, then you can supply the whole certificate chain as a character vector
#'   which begins at your own certificate and ends with
#'   the certificate of the CA.
#' @param validate Logical of length 1, whether to validate the configuration
#'   object on creation. If `FALSE`, then `validate()` can be called later on.
#' @examples
#' crew_tls(mode = "automatic")
crew_tls <- function(
  mode = "none",
  key = NULL,
  password = NULL,
  certificates = NULL,
  validate = TRUE
) {
  tls <- crew_class_tls$new(
    mode = mode,
    key = key,
    password = password,
    certificates = certificates
  )
  if (isTRUE(validate)) {
    tls$validate()
  }
  tls
}

#' @title `R6` TLS class.
#' @export
#' @family tls
#' @description `R6` class for TLS configuration.
#' @details See [crew_tls()].
#' @examples
#' crew_tls(mode = "automatic")
crew_class_tls <- R6::R6Class(
  classname = "crew_class_tls",
  cloneable = FALSE,
  private = list(
    .mode = NULL,
    .key = NULL,
    .password = NULL,
    .certificates = NULL,
    .validate_mode_automatic = function() {
      for (field in c(".key", ".password", ".certificates")) {
        crew_assert(
          is.null(private[[field]]),
          message = paste(
            "If mode is not \"custom\" in crew_tls(), then",
            field,
            "must be NULL."
          )
        )
      }
      invisible()
    },
    .validate_mode_custom = function() {
      crew_assert(
        private$.key,
        is.character(.),
        length(.) == 1L,
        nzchar(.),
        !anyNA(.),
        message = paste(
          "If mode is \"custom\", then crew_tls() argument key",
          "must be a nonempty nonmissing character of length 1."
        )
      )
      crew_assert(
        private$.password %|||% "x",
        is.character(.),
        length(.) == 1L,
        nzchar(.),
        !anyNA(.),
        message = paste(
          "If mode is \"custom\", then crew_tls() argument password",
          "must be NULL or a nonempty nonmissing character of length 1."
        )
      )
      crew_assert(
        private$.certificates,
        is.character(.),
        length(.) >= 1L,
        nzchar(.),
        !anyNA(.),
        message = paste(
          "If mode is \"custom\", then crew_tls() argument certificates",
          "must a nonempty nonmissing character vector of length >= 1."
        )
      )
      files <- c(private$.key, private$.certificates)
      for (file in files) {
        crew_assert(
          file.exists(file),
          message = paste("file not found:", file)
        )
      }
      crew_tls_assert_key(private$.key)
      for (certificate in private$.certificates) {
        crew_tls_assert_certificate(certificate)
      }
      invisible()
    },
    .read_files = function(files) {
      lines <- unlist(
        lapply(
          X = files,
          FUN = function(file) {
            readLines(file)
          }
        )
      )
      paste(lines, collapse = "\n")
    },
    .read_key = function() {
      private$.read_files(files = private$.key)
    },
    .read_certificates = function() {
      private$.read_files(files = private$.certificates)
    }
  ),
  active = list(
    #' @field mode See [crew_tls()].
    mode = function() {
      .subset2(private, ".mode")
    },
    #' @field key See [crew_tls()].
    key = function() {
      .subset2(private, ".key")
    },
    #' @field password See [crew_tls()].
    password = function() {
      .subset2(private, ".password")
    },
    #' @field certificates See [crew_tls()].
    certificates = function() {
      .subset2(private, ".certificates")
    }
  ),
  public = list(
    #' @description TLS configuration constructor.
    #' @return An `R6` object with TLS configuration.
    #' @param mode Argument passed from [crew_tls()].
    #' @param key Argument passed from [crew_tls()].
    #' @param password Argument passed from [crew_tls()].
    #' @param certificates Argument passed from [crew_tls()].
    #' @examples
    #' crew_tls(mode = "automatic")
    initialize = function(
      mode = NULL,
      key = NULL,
      password = NULL,
      certificates = NULL
    ) {
      private$.mode <- mode
      private$.key <- key
      private$.password <- password
      private$.certificates <- certificates
    },
    #' @description Validate the object.
    #' @return `NULL` (invisibly).
    #' @param test Logical of length 1, whether to test the TLS configuration
    #'   with `nanonext::tls_config()`.
    validate = function(test = TRUE) {
      crew_assert(
        private$.mode,
        is.character(.),
        length(.) == 1L,
        nzchar(.),
        !anyNA(.),
        . %in% c("none", "automatic", "custom"),
        message = paste(
          "crew_tls() argument mode",
          "must be \"none\", \"automatic\", or \"custom\"."
        )
      )
      if_any(
        private$.mode %in% c("none", "automatic"),
        private$.validate_mode_automatic(),
        private$.validate_mode_custom()
      )
      # Cannot test in unit tests because custom TLS configuration
      # is platform-dependent and low-level.
      # nocov start
      if (isTRUE(test)) {
        nanonext::tls_config(
          client = self$worker(name = "default"),
          server = self$client(),
          pass = private$.password
        )
      }
      # nocov end
      invisible()
    },
    #' @description TLS credentials for the `crew` client.
    #' @return `NULL` or character vector, depending on the mode.
    client = function() {
      if (private$.mode != "custom") {
        return(NULL)
      } else if (private$.mode == "custom") {
        return(c(private$.read_certificates(), private$.read_key()))
      }
    },
    #' @description TLS credentials for `crew` workers.
    #' @return `NULL` or character vector, depending on the mode.
    #' @param name Character of length 1 with the `mirai` compute profile.
    worker = function(name) {
      if (private$.mode == "none") {
        return(NULL)
      } else if (private$.mode == "automatic") {
        return(mirai::nextget(x = "tls", .compute = name))
      } else if (private$.mode == "custom") {
        return(c(private$.read_certificates(), ""))
      }
    }
  )
)

crew_tls_assert_key <- function(key) {
  crew_assert(
    file.exists(key),
    message = "private key file not found"
  )
  lines <- readLines(key)
  crew_assert(
    length(lines) > 0L,
    message = "private key file is empty"
  )
  crew_assert(
    lines[1L] == "-----BEGIN PRIVATE KEY-----" ||
      lines[1L] == "-----BEGIN ENCRYPTED PRIVATE KEY-----",
    message = paste(
      "private key file must begin with the line",
      "-----BEGIN PRIVATE KEY----- or -----BEGIN ENCRYPTED PRIVATE KEY-----.",
      "please make sure you have a valid private key in PEM format."
    )
  )
  crew_assert(
    lines[length(lines)] == "-----END PRIVATE KEY-----" ||
      lines[length(lines)] == "-----END ENCRYPTED PRIVATE KEY-----",
    message = paste(
      "private key file must end with the line",
      "-----END PRIVATE KEY----- or -----END ENCRYPTED PRIVATE KEY-----.",
      "please make sure you have a valid private key in PEM format."
    )
  )
}

crew_tls_assert_certificate <- function(certificate) {
  crew_assert(
    file.exists(certificate),
    message = paste("certificate file not found:", certificate)
  )
  lines <- readLines(certificate)
  crew_assert(
    length(lines) > 0L,
    message = paste("certificate file is empty:", certificate)
  )
  crew_assert(
    lines[1L] == "-----BEGIN CERTIFICATE-----",
    message = paste(
      "certificate file must begin with the line",
      "-----BEGIN CERTIFICATE-----.",
      "please make sure you have a valid certificate in PEM format."
    )
  )
  crew_assert(
    lines[length(lines)] == "-----END CERTIFICATE-----",
    message = paste(
      "certificate file must end with the line",
      "-----END CERTIFICATE-----.",
      "please make sure you have a valid certificate in PEM format."
    )
  )
}
