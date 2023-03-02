#' crew: a distributed worker launcher
#' @docType package
#' @name crew-package
#' @description In computationally demanding analysis workflows,
#'   statisticians and data scientists asynchronously deploy
#'   long-running tasks to distributed systems, ranging from
#'   traditional clusters to cloud services. The `crew` package
#'   extends existing task schedulers to run workers on these systems.
#'   With its unifying interface to multiple backends,
#'   `crew` resembles packages `clustermq`,
#'   `future`, and `batchtools`.
#' @family help
#' @importFrom callr r_session
#' @importFrom getip getip
#' @importFrom mirai daemons is_error_value mirai server
#' @importFrom R6 R6Class
#' @importFrom rlang abort as_function
#' @importFrom tibble tibble
#' @importFrom utils globalVariables
#' @importFrom withr local_options
NULL

utils::globalVariables(".")
