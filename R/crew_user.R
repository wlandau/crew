#' @title Get the user ID.
#' @export
#' @family utility
#' @description Get the user ID running the current R session.
#' @details First try `Sys.info()["user"]`. If the value is `"unknown"`,
#'   try `Sys.info()["effective_user"]`. If that values is also
#'   `"unknown"`, then the return value is `NULL`.
#' @return Character of length 1 with the user ID if it can be determined,
#'   `NULL` if the user ID cannot be determined.
crew_user <- function() {
  info <- Sys.info()
  user <- unname(info["user"])
  effective_user <- unname(info["effective_user"])
  user <- if_any(identical(tolower(user), "unknown"), effective_user, user)
  if_any(identical(tolower(user), "unknown"), NULL, user)
}
