library(crew)
controller <- crew_controller_local(workers = 20)
controller$start()
for (index in c(0L, 1L)) {
  out <- controller$map(
    command = {
      if (x <= 1000L) {
        stop(x)
      } else if (x <= 4000L) {
        warning(x)
      }
      x
    },
    iterate = list(x = seq_len(6000L)),
    error = "silent"
  )
  testthat::expect_equal(length(controller$list), 0L)
  sum <- controller$summary()
  testthat::expect_equal(sum(sum$errors), 1000L + index * 1000L)
  testthat::expect_equal(sum(sum$warnings), 3000L + index * 3000L)
  name <- as.integer(out$name)
  testthat::expect_equal(name, seq_len(6000L))
  testthat::expect_true(all((name <= 1000L) == !is.na(out$error)))
  testthat::expect_true(
    all(
      ((name > 1000L) & (name <= 4000L)) == !is.na(out$warnings)
    )
  )
}
controller$terminate()

# In case the above gets stuck again, work on the reprex below.
# nolint start
#
# library(mirai)
# library(nanonext)
# library(purrr)
# 
# daemons(n = 20, url = "ws://127.0.0.1:5700")
# walk(rownames(status()$daemons), launch_local)
# 
# cv1 <- nextget("cv")
# cv2 <- cv()
# cv1 %~>% cv2
# 
# n <- 20000
# 
# tasks <- replicate(n, mirai(TRUE))
# while(cv_value(cv1) < n) {
#   until(cv2, msec = 500)
# }
# all(!map_lgl(tasks, unresolved))
# 
# tasks <- replicate(n, mirai(TRUE))
# while(cv_value(cv1) < 2 * n) {
#   until(cv2, msec = 500)
# }
# all(!map_lgl(tasks, unresolved))
# 
# daemons(n = 0)
#
# nolint end