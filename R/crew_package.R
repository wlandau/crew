#' crew: abstract interface specification for semi-persistent
#'   high-performance computing workers
#' @docType package
#' @name crew-package
#' @description The `R6` classes of `crew` establish a standardized
#'   user interface to high-performance computing technologies.
#'   Unlike its closely related `future` package, `crew` prioritizes
#'   centralized scheduling, heterogeneous semi-persistent workers,
#'   and user-driven customization. The primary goal is to help
#'   pipeline tools such as such `targets` efficiently orchestrate
#'   tasks without having to support individual low-level interfaces
#'   to specific high-performance computing platforms or cloud services.
#' @family help
#' @importFrom digest digest
#' @importFrom openssl rand_bytes
#' @importFrom processx process
#' @importFrom R6 R6Class
#' @importFrom redux hiredis
#' @importFrom rrq rrq_worker
#' @importFrom rlang abort as_function
#' @importFrom utils globalVariables
#' @importFrom withr local_options
NULL

utils::globalVariables(".")
