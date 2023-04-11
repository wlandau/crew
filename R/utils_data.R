as_class <- function(x, class) {
  class(x) <- c(class, class(x))
  x
}

paste_list <- function(x) {
  if_any(
    length(x),
    paste0(names(x), ": ", x),
    list()
  )
}
