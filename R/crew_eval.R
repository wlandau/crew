#' @title Evaluate an R command and return results as a monad.
#' @export
#' @family utilities
#' @description Not a user-side function. Do not call directly.
#' @details The `crew_eval()` function evaluates an R expression
#'   in an encapsulated environment and returns a monad with the results,
#'   including warnings and error messages if applicable.
#'   The random number generator seed, `globals`, and global options
#'   are restored to their original values on exit.
#' @return A monad object with results and metadata.
#' @param command Language object with R code to run.
#' @param name Character of length 1, name of the task.
#' @param string Character of length 1, string representation of the command.
#' @param data Named list of local data objects in the evaluation environment.
#' @param globals Named list of objects to temporarily assign to the
#'   global environment for the task.
#' @param seed Integer of length 1 with the pseudo-random number generator
#'   seed to set for the evaluation of the task. Does not restore
#'   the original seed, but this is okay because `crew_eval()`
#'   should only run in a non-interactive worker process.
#' @param packages Character vector of packages to load for the task.
#' @param library Library path to load the packages. See the `lib.loc`
#'   argument of `require()`.
#' @examples
#' crew_eval(quote(1 + 1))
crew_eval <- function(
  command,
  name = NA_character_,
  string = NA_character_,
  data = list(),
  globals = list(),
  seed = as.integer(nanonext::random() / 2),
  packages = character(0),
  library = NULL
) {
  load_packages(packages = packages, library = library)
  set.seed(seed)
  list2env(x = globals, envir = globalenv())
  envir <- list2env(x = data, parent = globalenv())
  capture_error <- function(condition) {
    state$error <- crew_eval_message(condition)
    state$error_class <- class(condition)
    state$trace <- paste(as.character(sys.calls()), collapse = "\n")
    NULL
  }
  capture_warning <- function(condition) {
    state$count_warnings <- (state$count_warnings %||% 0L) + 1L
    should_store_warning <- (state$count_warnings < crew_eval_max_warnings) &&
      (nchar(state$warnings %||% "") < crew_eval_max_nchar)
    if (should_store_warning) {
      state$warnings <- paste(
        c(state$warnings, crew_eval_message(condition)),
        collapse = ". "
      )
      state$warnings <- substr(
        state$warnings,
        start = 0,
        stop = crew_eval_max_nchar
      )
    }
    invokeRestart("muffleWarning")
  }
  state <- new.env(hash = FALSE, parent = emptyenv())
  start <- nanonext::mclock()
  result <- tryCatch(
    expr = withCallingHandlers(
      expr = eval(expr = command, envir = envir),
      error = capture_error,
      warning = capture_warning
    ),
    error = function(condition) NA
  )
  seconds <- (nanonext::mclock() - start) / 1000
  monad_init(
    name = name,
    command = string,
    result = result,
    seconds = seconds,
    seed = seed,
    error = state$error %|||% NA_character_,
    trace = state$trace %|||% NA_character_,
    warnings = state$warnings %|||% NA_character_,
    launcher = Sys.getenv("CREW_LAUNCHER", unset = NA_character_),
    worker = as.integer(Sys.getenv("CREW_WORKER", unset = NA_character_)),
    instance = Sys.getenv("CREW_INSTANCE", unset = NA_character_)
  )
}

crew_eval_message <- function(condition, prefix = character(0)) {
  out <- crew_eval_text_substring(
    message = conditionMessage(condition),
    prefix = prefix
  )
  if_any(nzchar(out), out, ".")
}

crew_eval_text_substring <- function(message, prefix = character(0)) {
  tryCatch(
    substr(
      paste(c(prefix, message), collapse = " "),
      start = 0L,
      stop = crew_eval_max_nchar
    ),
    error = function(condition) {
      paste(
        "crew could not process the error or warning message",
        "due to a text encoding issue."
      )
    }
  )
}

crew_eval_max_nchar <- 2048L
crew_eval_max_warnings <- 51L
