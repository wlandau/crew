#' crew: a distributed worker launcher framework
#' @docType package
#' @name crew-package
#' @description In computationally demanding analysis projects,
#'   statisticians and data scientists asynchronously deploy
#'   long-running tasks to distributed systems, ranging from
#'   traditional clusters to cloud services.
#'   The [NNG](https://nng.nanomsg.org)-powered
#'   [`mirai`](https://github.com/shikokuchuo/mirai)
#'   R package is a sleek and sophisticated scheduler
#'   that efficiently processes these intense workloads.
#'   The `crew` package extends
#'   [`mirai`](https://github.com/shikokuchuo/mirai)
#'   with a unifying interface
#'   for third-party worker launchers.
#'   Inspiration also comes from packages
#'   [`future`](https://future.futureverse.org/),
#'   [`rrq`](https://mrc-ide.github.io/rrq/),
#'   [`clustermq`](https://mschubert.github.io/clustermq/),
#'   and [`batchtools`](https://mllg.github.io/batchtools/).
#' @family help
#' @importFrom getip getip
#' @importFrom mirai daemons mirai saisei server
#' @importFrom nanonext mclock msleep random sha1 .unresolved
#' @importFrom processx process
#' @importFrom ps ps_handle ps_is_running ps_kill
#' @importFrom R6 R6Class
#' @importFrom rlang abort as_function enquo is_installed quo_squash
#' @importFrom stats runif
#' @importFrom tibble as_tibble new_tibble tibble
#' @importFrom tidyselect all_of any_of contains ends_with eval_select
#'   everything last_col matches num_range one_of starts_with
#' @importFrom utils globalVariables head
NULL

utils::globalVariables(".")
