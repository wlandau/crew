#' crew: a distributed worker launcher framework
#' @name crew-package
#' @family help
#' @description In computationally demanding analysis projects,
#'   statisticians and data scientists asynchronously deploy
#'   long-running tasks to distributed systems, ranging from
#'   traditional clusters to cloud services.
#'   The [NNG](https://nng.nanomsg.org)-powered
#'   [`mirai`](https://github.com/r-lib/mirai)
#'   R package is a sleek and sophisticated scheduler
#'   that efficiently processes these intense workloads.
#'   The `crew` package extends
#'   [`mirai`](https://github.com/r-lib/mirai)
#'   with a unifying interface
#'   for third-party worker launchers.
#'   Inspiration also comes from packages
#'   [`future`](https://future.futureverse.org/),
#'   [`rrq`](https://mrc-ide.github.io/rrq/),
#'   [`clustermq`](https://mschubert.github.io/clustermq/),
#'   and [`batchtools`](https://github.com/mlr-org/batchtools).
#' @importFrom cli cli_progress_bar cli_progress_done cli_progress_update
#' @importFrom collections dict queue
#' @importFrom data.table rbindlist
#' @importFrom later create_loop current_loop destroy_loop exists_loop later
#' @importFrom mirai call_mirai daemon daemons info is_mirai
#'   is_mirai_error mirai nextcode nextget nextstream stop_mirai
#' @importFrom nanonext %~>% cv ip_addr mclock msleep nng_error
#'   random .unresolved until_ tls_config
#' @importFrom processx process
#' @importFrom promises promise
#' @importFrom ps ps_cmdline ps_handle ps_pid ps_is_running
#'   ps_username
#' @importFrom R6 R6Class
#' @importFrom rlang abort as_function enquo is_installed is_named quo_squash
#' @importFrom stats runif
#' @importFrom tibble as_tibble new_tibble tibble
#' @importFrom tidyselect all_of any_of contains ends_with eval_select
#'   everything last_col matches num_range one_of starts_with
#' @importFrom tools pskill SIGINT SIGQUIT SIGTERM
#' @importFrom utils capture.output compareVersion globalVariables head
#'   sessionInfo tail
NULL

utils::globalVariables(".")
