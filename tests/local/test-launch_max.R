# Run manually and watch htop.
# Pause between launches to allow workers to idle out.
library(crew)
library(testthat)
x <- crew_controller_local(
  workers = 1L,
  seconds_idle = 1e-9,
  launch_max = 3L
)
x$start()
expect_equal(x$launcher$workers$futile, 0L)
x$launch(n = 1L)
# Pause until worker idles out.
Sys.sleep(5)
expect_equal(x$launcher$workers$futile, 0L)
x$launch(n = 1L)
# Pause until worker idles out.
Sys.sleep(5)
expect_equal(x$launcher$workers$futile, 1L)
private <- crew_private(x$launcher)
private$.seconds_idle <- Inf
x$push(TRUE)
x$wait()
expect_equal(x$launcher$workers$futile, 2L)
x$launcher$terminate_workers(index = 1L)
# Pause until worker exits.
Sys.sleep(5)
private$.seconds_idle <- 1e-9
x$launch(n = 1L)
# Pause until worker exits.
Sys.sleep(5)
expect_equal(x$launcher$workers$futile, 0L)
x$launch(n = 1L)
# Pause until worker exits.
Sys.sleep(5)
expect_equal(x$launcher$workers$futile, 1L)
x$launch(n = 1L)
# Pause until worker exits.
Sys.sleep(5)
expect_equal(x$launcher$workers$futile, 2L)
x$launch(n = 1L)
# Pause until worker exits.
Sys.sleep(5)
expect_equal(x$launcher$workers$futile, 3L)
expect_error(x$launch(n = 1L), class = "crew_error")
x$terminate()
