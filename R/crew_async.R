#' @title Run an asynchronous task in the crew launcher.
#' @description Called internally, not for users.
#' @export
#' @family utilities
#' @keywords internal
#' @return The result of running `command`.
#' @param command Language object with R code to run.
#' @param args Named list of objects that `command` depends on.
#' @param packages Character vector of packages to load.
#' @param library Character vector of library paths to load the packages from.
crew_async <- function(
  command,
  args,
  packages = character(0L),
  library = NULL
) {
  load_packages(packages = packages, library = library)
  eval(expr = command, envir = list2env(args, parent = globalenv()))
}

expr_crew_async <- quote(
  crew::crew_async(
    command = command,
    args = args,
    packages = packages,
    library = library
  )
)
