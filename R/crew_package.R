#' crew: abstract interface specification for semi-persistent
#'   high-performance computing workers
#' @docType package
#' @name crew-package
#' @description The `R6` classes of `crew` establish a standardized
#'   user interface to high-performance computing technologies.
#'   Unlike its closely related `future` package, `crew` prioritizes
#'   centralized scheduling, semi-persistent (reusable) workers,
#'   and user-driven customization. The primary goal is to help
#'   pipeline tools such as such `targets` efficiently orchestrate
#'   tasks without having to support individual low-level interfaces
#'   to specific high-performance computing platforms or cloud services.
#' @family help
#' @importFrom fs file_create
#' @importFrom qs qsave
#' @importFrom R6 R6Class
#' @importFrom rlang abort
NULL
