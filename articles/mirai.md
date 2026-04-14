# mirai workflows

`crew` is an all-inclusive wrapper around `mirai` to manage workers and
tasks from one place. However, you can let `mirai` manage the tasks and
just use `crew` to manage workers. With `crew`’s customizable [launcher
plugins](https://wlandau.github.io/crew/articles/plugins.md) system,
along with the pre-built plugins in `crew.aws.batch` and `crew.cluster`,
you can deploy your `mirai` tasks to a wide range of computing
environments.

## How it works

First, create a `crew` controller with “default” compute profile and
`seconds_idle = Inf`.[¹](#fn1)

``` r
library(crew)
controller <- crew_controller_local(profile = "default", seconds_idle = Inf)
```

Next, launch one or more workers.

``` r
controller$launch(n = 1)
```

Submit a `mirai` task normally.[²](#fn2)

``` r
library(mirai)
task <- mirai(1 + 1)
```

The task will start as soon as the worker connects to the controller.
When the task completes, you can get the result with:

``` r
task$data
#> [1] 2
```

Below, a “completed” count greater than zero confirms that the task
actually ran on the controller.[³](#fn3)

``` r
controller$client$status()
#> connections  cumulative    awaiting   executing   completed 
#>           1           1           0           0           1 
```

To stop the workers, either close the local R session or terminate the
controller.

``` r
controller$terminate()
```

## Parallel functional programming

The pattern is the same with `mirai`-powered parallel purrr. First,
create the controller and launch the workers.

``` r
library(crew)
controller <- crew_controller_local(profile = "default", seconds_idle = Inf)
controller$launch(n = 4)
```

Then, use the controller’s compute profile in `mirai`’s parallel purrr
functions.

``` r
library(purrr)
seq_len(4) |> map(in_parallel(\(x) Sys.sleep(1))) # Takes 1 second to run.
```

## Asynchronous parallel functional programming

[`mirai::mirai_map()`](https://mirai.r-lib.org/reference/mirai_map.html)
schedules functional programming tasks without blocking the R session.
The pattern is analogous to the `purrr` case.

``` r
controller <- crew_controller_local(profile = "default", seconds_idle = Inf)
controller$launch(n = 4)
tasks <- mirai_map(seq_len(4), \(x) Sys.sleep(10))
tasks
#> < mirai map [0/4] > # The tasks are still running.
```

## Auto-scaling

`crew` can automatically scale workers in response to demand from
`mirai` tasks. To enable this, we configure the controller differently:

``` r
controller <- crew_controller_local(
  profile = "default",
  seconds_idle = 30, # Workers will terminate after 30 seconds of idleness.
  workers = 4        # No more than 4 workers will run at one time.
)
```

The `autoscale()` method runs an asynchronous `later` loop that launches
new workers in the background.

``` r
controller$autoscale()
```

`later` autoscaling not compatible with either of the functional
programming sections above, but it can accommodate individual tasks.

``` r
task <- mirai(1 + 1)
# After waiting a few seconds:
task$data
#> [1] 2
```

To deactivate the auto-scaling loop:

``` r
controller$descale()
```

## Caveats and limitations

- If you follow the patterns in this vignette, do not submit or collect
  tasks directly through the controller (e.g. controller methods
  `push()`, [`map()`](https://purrr.tidyverse.org/reference/map.html),
  [`walk()`](https://purrr.tidyverse.org/reference/map.html), `pop()`,
  or `collect()`). Those methods rely on the task counters in
  `controller$client$status()` (from
  [`mirai::info()`](https://mirai.r-lib.org/reference/info.html)), which
  increment with every task in the compute profile, regardless of how
  the task was submitted. If you submit any tasks outside the controller
  (e.g. through \`mirai::mirai()), then you must submit and collect all
  other tasks outside the controller as well.
- Due to the constraints of `later`, the auto-scaling `later` loop is
  only compatible with individually-launched tasks at the top level of
  the call stack (outside function calls) or in Shiny apps.
  `controller$autoscale()` will not work with parallel `purrr` or
  [`mirai_map()`](https://mirai.r-lib.org/reference/mirai_map.html)
  unless those functions manually call
  [`later::run_now()`](https://later.r-lib.org/reference/run_now.html)
  to trigger `crew`’s auto-scaling.

------------------------------------------------------------------------

1.  Or a compute profile you will supply to the `.compute` argument of
    [`mirai::mirai()`](https://mirai.r-lib.org/reference/mirai.html).

2.  If you didn’t set the “default” compute profile in the controller,
    you can set it in
    [`mirai()`](https://mirai.r-lib.org/reference/mirai.html) with
    `.compute = controller$profile`.

3.  These counts are the result of
    `mirai::info(.compute = controller$profile)`. The “connections” and
    “cumulative” counts are for workers, and the rest are for tasks.
