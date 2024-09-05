#' @title Evaluate an R command and return results as a monad.
#' @export
#' @family utility
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
#'   seed to set for the evaluation of the task. Passed to the
#'   `seed` argument of `set.seed()` if not `NULL`.
#'   If `algorithm` and `seed` are both `NULL`,
#'   then the random number generator defaults to the
#'   recommended widely spaced worker-specific
#'   L'Ecuyer streams as supported by `mirai::nextstream()`.
#'   See `vignette("parallel", package = "parallel")` for details.
#' @param algorithm Integer of length 1 with the pseudo-random number
#'   generator algorithm to set for the evaluation of the task.
#'   Passed to the `kind` argument of `RNGkind()` if not `NULL`.
#'   If `algorithm` and `seed` are both `NULL`,
#'   then the random number generator defaults to the
#'   recommended widely spaced worker-specific
#'   L'Ecuyer streams as supported by `mirai::nextstream()`.
#'   See `vignette("parallel", package = "parallel")` for details.
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
  seed = NULL,
  algorithm = NULL,
  packages = character(0),
  library = NULL
) {
  old_algorithm <- RNGkind()[1L]
  old_seed <- .subset2(.GlobalEnv, ".Random.seed")
  if (!is.null(algorithm) || !is.null(seed)) {
    if (!is.null(algorithm)) {
      RNGkind(kind = algorithm)
    }
    if (!is.null(seed)) {
      set.seed(seed = seed)
    }
    on.exit(RNGkind(kind = old_algorithm))
    on.exit(.GlobalEnv$.Random.seed <- old_seed, add = TRUE)
  }
  load_packages(packages = packages, library = library)
  list2env(x = globals, envir = globalenv())
  envir <- list2env(x = data, parent = globalenv())
  capture_error <- function(condition) {
    message(paste("Error:", conditionMessage(condition))) # for log files
    state$error <- crew_eval_message(condition)
    state$error_class <- class(condition)
    state$trace <- paste(as.character(sys.calls()), collapse = "\n")
    NULL
  }
  capture_warning <- function(condition) {
    message(paste("Warning:", conditionMessage(condition))) # for log files
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
    seed = seed %|||% NA_integer_,
    algorithm = algorithm %|||% NA_character_,
    status = if_any(is.null(state$error), "success", "error"),
    code = as.integer(!is.null(state$error)),
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

expr_crew_eval <- quote(
  crew::crew_eval(
    name = name,
    command = command,
    string = string,
    data = data,
    globals = globals,
    seed = seed,
    algorithm = algorithm,
    packages = packages,
    library = library
  )
)
