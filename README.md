
# crew: a distributed worker launcher <img src='man/figures/logo-readme.png' align="right" height="139"/>

[![CRAN](https://www.r-pkg.org/badges/version/crew)](https://CRAN.R-project.org/package=crew)
[![status](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#Active)
[![check](https://github.com/wlandau/crew/workflows/check/badge.svg)](https://github.com/wlandau/crew/actions?query=workflow%3Acheck)
[![codecov](https://codecov.io/gh/wlandau/crew/branch/main/graph/badge.svg?token=3T5DlLwUVl)](https://app.codecov.io/gh/wlandau/crew)
[![lint](https://github.com/wlandau/crew/workflows/lint/badge.svg)](https://github.com/wlandau/crew/actions?query=workflow%3Alint)

In computationally demanding analysis projects, statisticians and data
scientists asynchronously deploy long-running tasks to distributed
systems, ranging from traditional clusters to cloud services. The
[NNG](https://nng.nanomsg.org)-powered
[`mirai`](https://github.com/shikokuchuo/mirai) R package is a sleek and
sophisticated scheduler that efficiently processes these intense
workloads. The `crew` package extends
[`mirai`](https://github.com/shikokuchuo/mirai) with a unifying
interface for third-party worker launchers. Inspiration also comes from
packages [`future`](https://future.futureverse.org/),
[`rrq`](https://mrc-ide.github.io/rrq/),
[`clustermq`](https://mschubert.github.io/clustermq/), and
[`batchtools`](https://mllg.github.io/batchtools/).

# Installation

| Type        | Source     | Command                                                              |
|-------------|------------|----------------------------------------------------------------------|
| Release     | CRAN       | `install.packages("crew")`                                           |
| Development | GitHub     | `remotes::install_github("wlandau/crew")`                            |
| Development | R-universe | `install.packages("crew", repos = "https://wlandau.r-universe.dev")` |

# Documentation

Please see <https://wlandau.github.io/crew/> for documentation,
including a full function reference and usage tutorial vignettes.

# Plugins

`crew` lets you write custom
[launchers](https://wlandau.github.io/crew/reference/crew_class_launcher.html)
for different types of workers that connect over the local network. This
flexibility can extend `crew` to platforms like
[SLURM](https://slurm.schedmd.com/), [AWS
Batch](https://aws.amazon.com/batch/), and
[Kubernetes](https://kubernetes.io/). See the [plugin
vignette](https://wlandau.github.io/crew/articles/plugins.html) for
details.

# Usage

First, create a controller object. Thanks to the powerful features in
[`mirai`](https://github.com/shikokuchuo/mirai),
`crew_controller_local()` allows several ways to customize the way
workers are launched and the conditions under which they time out. For
example, arguments `tasks_max` and `seconds_idle` allow for a smooth
continuum between fully persistent workers and fully transient workers.

``` r
library(crew)
controller <- crew_controller_local(
  workers = 2,
  tasks_max = 3,
  auto_scale = "demand"
)
```

The `start()` method starts a local
[`mirai`](https://github.com/shikokuchuo/mirai) client and dispatcher
process to listen to workers that dial in into websockets on the local
network.

``` r
controller$start()
```

The `summary()` method shows metadata on workers and tasks. Use the
`columns` argument to select a subset of columns in the output. The
following table has one row per worker, and each column is a summary
metric on the popped tasks (tasks which completed and were retreived
with `pop()`).

``` r
controller$summary(columns = starts_with("popped_"))
#> # A tibble: 2 × 4
#>   popped_tasks popped_seconds popped_errors popped_warnings
#>          <int>          <dbl>         <int>           <int>
#> 1            0              0             0               0
#> 2            0              0             0               0
```

Use the `push()` method to submit a task. When you do, `crew`
automatically scales up the number of workers to meet demand, within the
constraints of the `auto_scale` and `workers` arguments of
`crew_controller_local()`.

``` r
controller$push(name = "get worker process ID", command = ps::ps_pid())
```

You can retieve the results of a completed task, if available, with
`pop()`. Like `push()`, `pop()` also tries to launch workers for pending
tasks that need them. The return value is `NULL` if no new tasks are
complete.

``` r
controller$pop()
#> NULL
```

If you expect a result but see `NULL`, then `mirai` may not have
assigned the task to a worker yet. This can happen if all workers
self-terminate early according to the `tasks_max` and `seconds_idle`
arguments of `crew_controller_local()`. Unless you configured the
workers to run indefinitely, the controller will need ongoing attention
to work through the backlog of tasks. To promptly re-scale workers to
meet demand, you can manually repeat calls to `pop()`, or you can call
`wait()`. The `wait()` method blocks the R session, repeatedly scales
workers, and collects completed tasks.

``` r
controller$wait(mode = "all")
```

When a result is available, `pop()` will retrieve it.

``` r
out <- controller$pop()
```

The result is a
[monad](https://en.wikipedia.org/wiki/Monad_(functional_programming))
with the result and its metadata. Even if the command of the task throws
an error, it will still return the same kind of
[monad](https://en.wikipedia.org/wiki/Monad_(functional_programming)).

``` r
out
#> # A tibble: 1 × 11
#>   name         command result seconds   seed error trace…¹ warni…² launc…³ worker insta…⁴
#>   <chr>        <chr>   <list>   <dbl>  <int> <chr> <chr>   <chr>   <chr>    <int> <chr>  
#> 1 get worker … ps::ps… <int>        0 1.56e8 NA    NA      NA      79e71c…      1 7686b2…
#> # … with abbreviated variable names ¹​traceback, ²​warnings, ³​launcher, ⁴​instance
```

The return value of the command is available in the `result` column. In
our case, it is the process ID of the parallel worker that ran it, as
reported by `ps::ps_pid()`.

``` r
out$result[[1]] # process ID of the parallel worker reported by the task
#> [1] 69631
```

Since it ran on a parallel worker, it is different from the process ID
of the local R session.

``` r
ps::ps_pid() # local R session process ID
#> [1] 69523
```

Continue the above process of asynchronously submitting and collecting
tasks until your workflow is complete. You may periodically inspect
different columns from the `summary()` method.

``` r
controller$summary(columns = starts_with("tasks"))
#> # A tibble: 2 × 2
#>   tasks_assigned tasks_complete
#>            <int>          <int>
#> 1              1              1
#> 2              0              0
```

``` r
controller$summary(columns = starts_with("popped"))
#> # A tibble: 2 × 4
#>   popped_tasks popped_seconds popped_errors popped_warnings
#>          <int>          <dbl>         <int>           <int>
#> 1            1              0             0               0
#> 2            0              0             0               0
```

When you are done, terminate the controller to close any workers still
running, close the [`mirai`](https://github.com/shikokuchuo/mirai)
dispatcher process, and free the TCP port.

``` r
controller$terminate()
```

# Risks

The `crew` package has unavoidable risk. It is your responsibility as
the user to safely use `crew`. Please read the final clause of the
[software license](https://wlandau.github.io/crew/LICENSE.html).

### Security

`crew` currently uses unencrypted TCP connections for transactions with
workers inside a trusted local network. In a compromised network, an
attacker can potentially access and exploit sensitive resources. It is
your responsibility to assess the sensitivity and vulnerabilities of
your computing environment and make sure your network is secure.

### Ports

`crew` uses one TCP port per controller. TCP ports range from 0 to
65535, and only around 16000 of these ports are considered ephemeral or
dynamic, so please be careful not to run too many controllers
simultaneously if you are running R on a machine you share with other
people (such as the login node of a computing cluster). If you are
running a [controller
group](https://wlandau.github.io/crew/articles/controller_groups.html)
please add only a small number of controllers to the group. The
`terminate()` method of the controller and `crew_session_terminate()`
should free these ports again for other processes to use.

### Zombies

The `crew` package launches external R processes:

1.  Worker processes to run tasks, possibly on different computers on
    the local network, and
2.  A local [`mirai`](https://github.com/shikokuchuo/mirai) dispatcher
    process to schedule the tasks.

To the best of its ability, `crew` tries to only launch the processes it
needs, and it relies on `mirai` to clean up these processes when the
work is done. However, sometimes it is still possible that too many
workers may run concurrently, and it is still possible that either the
workers or the [`mirai`](https://github.com/shikokuchuo/mirai)
dispatcher may run too long or hang. In large-scale workflows, these
accidents can have egregious consequences. Depending on the launcher
type, these consequences can range from overburdening your local machine
or cluster, to incurring unexpectedly high costs on [Amazon Web
Services](https://aws.amazon.com/).

### Workers

`mirai` usually terminates workers when they are no longer needed, but
sometimes it cannot reach a worker, e.g. when a worker comes online
after its startup time from `seconds_start` elapses. To make sure
workers do not run indefinitely if something goes wrong, it is always
prudent to set arguments like `seconds_idle` in functions like
`crew_controller_local()`. In addition, please learn how to find and
terminate workers on the specific computing platform where they run. And
if you are writing a custom launcher plugin, it is recommended (although
not strictly required) to write a custom `terminate_worker()` method.

Workers may run on different computing platforms, depending on the type
of launcher you choose. Each type of launcher connects to a different
computing platform, and each platform has a different way of terminating
workers. For example, the [local process
launcher](https://wlandau.github.io/crew/reference/crew_launcher_local.html)
creates R processes on your local machine, which you can find and
terminate with
[`ps::ps()`](https://ps.r-lib.org/reference/ps.html)/[`ps::ps_kill()`](https://ps.r-lib.org/reference/ps_kill.html)
or [`htop`](https://htop.dev/). For a SLURM launcher, you need
[`squeue`](https://slurm.schedmd.com/squeue.html) to find workers and
[`scancel`](https://slurm.schedmd.com/scancel.html) to terminate them.
For an [Amazon Web Services](https://aws.amazon.com/) launcher, please
use the [AWS web console](https://aws.amazon.com/console/) or
[CloudWatch](https://aws.amazon.com/cloudwatch/).

### Scheduling

Depending on user settings, `crew` workers may run for the entire length
of the analysis pipeline, or they may exit if they idle too long or
complete a certain number of tasks. `crew` re-launches workers if there
are more unfinished tasks in the queue than [active
workers](https://wlandau.github.io/crew/reference/crew_class_launcher.html#details)
to run them at a given snapshot in time. This kind of auto-scaling does
not dedicate any specific worker to any specific task, and it does not
perform well when workers exit too quickly due to a small value of
`seconds_idle`. Please set `seconds_idle` to a generous enough value for
workers to accept work, and please use `tasks_max` to specify
short-lived workers such as single-task transient workers
(`tasks_max = 1`).

### Dispatcher

The [`mirai`](https://github.com/shikokuchuo/mirai) dispatcher is
designed to gracefully exit when you call `terminate()` on the
controller object or when you restart your R session. However, if you
ever need to shut down the dispatcher manually, you can find the process
ID using the controller object, then use `ps::ps_kill()` to terminate
the process.

``` r
controller$router$dispatcher
#> [1] 86028
handle <- ps::ps_handle(pid = 86028L)
ps::ps_is_running(handle)
#> [1] TRUE
ps::ps_kill(handle)
ps::ps_is_running(handle)
#> [1] FALSE
```

# Similar work

- [`mirai`](https://github.com/shikokuchuo/mirai): a powerful R
  framework for asynchronous tasks built on
  [NNG](https://nng.nanomsg.org). The purpose of `crew` is to extend
  [`mirai`](https://github.com/shikokuchuo/mirai) to different computing
  platforms for distributed workers.
- [`rrq`](https://mrc-ide.github.io/rrq/): a task queue for R based on
  [Redis](https://redis.io).
- [`rrqueue`](http://traitecoevo.github.io/rrqueue/): predecessor of
  [`rrq`](https://mrc-ide.github.io/rrq/).
- [`clustermq`](https://mschubert.github.io/clustermq/): sends R
  function calls as jobs to computing clusters.
- [`future`](https://future.futureverse.org/): a unified interface for
  asynchronous evaluation of single tasks and map-reduce calls on a wide
  variety of backend technologies.
- [`batchtools`](https://mllg.github.io/batchtools/): tools for
  computation on batch systems.
- [`targets`](https://docs.ropensci.org/targets/): a Make-like pipeline
  tool for R.
- [`later`](https://r-lib.github.io/later/): delayed evaluation of
  synchronous tasks.
- [`promises`](https://rstudio.github.io/promises/): minimally-invasive
  asynchronous programming for a small number of tasks within Shiny
  apps.
- [`callr`](https://github.com/r-lib/callr): initiates R process from
  other R processes.
- [High-performance computing CRAN task
  view](https://CRAN.R-project.org/view=HighPerformanceComputing).

# Thanks

The `crew` package incorporates insightful ideas from the following
people.

- [Charlie Gao](https://github.com/shikokuchuo) created
  [`mirai`](https://github.com/shikokuchuo/mirai) and
  [`nanonext`](https://github.com/shikokuchuo/nanonext) and graciously
  accommodated the complicated and demanding feature requests that made
  `crew` possible.
- [Rich FitzJohn](https://github.com/richfitz) and [Robert
  Ashton](https://github.com/r-ash) developed
  [`rrq`](https://mrc-ide.github.io/rrq//).
- [Gábor Csárdi](https://github.com/gaborcsardi/) developed
  [`callr`](https://github.com/r-lib/callr) and wrote an [edifying blog
  post on implementing task
  queues](https://www.tidyverse.org/blog/2019/09/callr-task-q/).
- [Kirill Müller](https://github.com/krlmlr/) created the
  [`workers`](https://github.com/wlandau/workers) prototype, an initial
  effort that led directly to the current implementation of `crew`.
  `crew` would not exist without Kirill’s insights about orchestration
  models for R processes.
- [Henrik Bengtsson](https://github.com/HenrikBengtsson/). Henrik’s
  [`future`](https://github.com/HenrikBengtsson/future/) package
  ecosystem demonstrates the incredible power of a consistent R
  interface on top of a varying collection of high-performance computing
  technologies.
- [Michael Schubert](https://github.com/mschubert/). Michael’s
  [`clustermq`](https://mschubert.github.io/clustermq/) package supports
  efficient high-performance computing on traditional clusters, and it
  demonstrates the value of a central `R6` object to manage an entire
  collection of persistent workers.
- [David Kretch](https://github.com/davidkretch). The
  [`paws`](https://github.com/paws-r/paws) R package is a powerful
  interface to Amazon Web Services, and the documentation clearly
  communicates the capabilities and limitations of AWS to R users.
- [Adam Banker](https://github.com/adambanker), co-authored
  [`paws`](https://github.com/paws-r/paws) with [David
  Kretch](https://github.com/davidkretch).
- [David Neuzerling](https://github.com/mdneuzerling). David’s
  [`lambdr`](https://github.com/mdneuzerling/lambdr/) package
  establishes a helpful pattern to submit and collect AWS Lambda jobs
  from R.
- [Mark Edmondson](https://github.com/MarkEdmondson1234/). Mark
  maintains several R packages to interface with Google Cloud Platform
  such as
  [`googleCloudStorageR`](https://github.com/cloudyr/googleCloudStorageR)
  and
  [`googleCloudRunner`](https://github.com/MarkEdmondson1234/googleCloudRunner),
  and he [started the
  conversation](https://github.com/ropensci/targets/issues/720) around
  helping [`targets`](https://github.com/ropensci/targets) submit jobs
  to Google Cloud Run.

## Code of Conduct

Please note that the `crew` project is released with a [Contributor Code
of
Conduct](https://github.com/wlandau/crew/blob/main/CODE_OF_CONDUCT.md).
By contributing to this project, you agree to abide by its terms.

## Citation

``` r
citation("crew")
```
