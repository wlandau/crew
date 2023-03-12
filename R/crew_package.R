#' crew: a distributed worker launcher
#' @docType package
#' @name crew-package
#' @description In computationally demanding analysis workflows,
#'   statisticians and data scientists asynchronously deploy
#'   long-running tasks to distributed systems, ranging from
#'   traditional clusters to cloud services.
#'   The [NNG](https://nng.nanomsg.org)-powered
#'   [`mirai`](https://github.com/shikokuchuo/mirai) R package
#'   is a powerful task scheduler that efficiently processes
#'   these intense workloads. The role of the `crew` package
#'   is merely to extend [`mirai`](https://github.com/shikokuchuo/mirai)
#'   to computing platforms beyond local multicore processing.
#'   With its unifying interface to multiple backends,
#'   `crew` resembles packages
#'   [`clustermq`](https://mschubert.github.io/clustermq/),
#'   [`future`](https://future.futureverse.org/), and
#'   [`batchtools`](https://mllg.github.io/batchtools/).
#' @family help
#' @importFrom bench hires_time
#' @importFrom callr r_bg
#' @importFrom getip getip
#' @importFrom mirai daemons is_error_value mirai server unresolved
#' @importFrom nanonext socket stat
#' @importFrom parallelly freePort
#' @importFrom R6 R6Class
#' @importFrom rlang abort as_function
#' @importFrom tibble tibble
#' @importFrom utils globalVariables head
#' @importFrom uuid UUIDgenerate
#' @importFrom withr local_options
NULL

utils::globalVariables(".")
