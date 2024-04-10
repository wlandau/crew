# The ideas for how to implement the promises below
# are covered in the documentation of promises::promise() at
# https://rstudio.github.io/promises/reference/promise.html,
# a blogpost by Yihui Xie at https://yihui.org/en/2017/10/later-recursion/
# on the recursive use of later::later(), and conversations with
# Charlie Gao (Hibiki AI, github.com/shikokuchuo) and
# Joe Cheng (Posit PBC, github.com/jcheng5).
# Tested in tests/interactive/test-promises.R.
# nocov start
controller_promise <- function(
  controller,
  mode,
  seconds_interval,
  controllers
) {
  crew_assert(
    mode,
    identical(., "one") || identical(., "all"),
    message = "'mode' in promise() must be \"one\" or \"all\"."
  )
  crew_assert(
    seconds_interval,
    is.numeric(.),
    length(.) == 1L,
    is.finite(.),
    . > 0,
    message = "seconds_interval must be a finite positive number."
  )
  action <- function(resolve, reject) {
    poll <- function() {
      if (!controller$started(controllers = controllers)) {
        return(invisible())
      }
      mode_one <- identical(mode, "one")
      ready <- if_any(
        mode_one,
        .subset2(controller, "unpopped")() > 0L,
        .subset2(controller, "unresolved")() < 1L
      )
      if (ready) {
        result <- if_any(
          mode_one,
          controller$pop(scale = FALSE, controllers = controllers),
          controller$collect(scale = FALSE, controllers = controllers)
        )
        if_any(
          all(is.na(result$error)),
          resolve(result),
          reject(result$error[!is.na(result$error)][1L])
        )
      } else {
        later::later(poll, delay = seconds_interval)
      }
    }
    poll()
  }
  promises::promise(action = action)
}
# nocov end
