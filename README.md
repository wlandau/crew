
# crew: a distributed worker launcher framework <img src='man/figures/logo-readme.png' align="right" height="139"/>

[![CRAN](https://www.r-pkg.org/badges/version/crew)](https://CRAN.R-project.org/package=crew)
[![status](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
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

# 📣 Request for community contributions 📣

**The [launcher plugin
framework](https://wlandau.github.io/crew/articles/plugins.html) aims to
extend `crew` to modern platforms and services like [AWS
Batch](https://aws.amazon.com/batch/), [Google Cloud
Run](https://cloud.google.com/run/),
[Kubernetes](https://kubernetes.io/), and beyond. At the time of
writing, plugins for cloud computing do not yet exist. So if you have
access to these services and know how to use them, please consider
contributing a package with plugins of your own. The tutorial at
<https://wlandau.github.io/crew/articles/plugins.html> explains how. The
maintainer of `crew` would love to work with you\!**

# Table of Contents

1.  [Installation](#installation)
2.  [Documentation](#documentation)
3.  [Concepts](#concepts)
4.  [Plugins](#plugins)
5.  [Tasks](#tasks)
6.  [Functional programming](#functional-programming)
7.  [Summaries](#summaries)
8.  [Termination](#termination)
9.  [Scale](#scale)
10. [Risks](#risks)
11. [Similar work](#similar-work)
12. [Thanks](#thanks)
13. [Code of conduct](#code-of-conduct)
14. [Citation](#citation)

# Installation

| Type        | Source     | Command                                                              |
| ----------- | ---------- | -------------------------------------------------------------------- |
| Release     | CRAN       | `install.packages("crew")`                                           |
| Development | GitHub     | `remotes::install_github("wlandau/crew")`                            |
| Development | R-universe | `install.packages("crew", repos = "https://wlandau.r-universe.dev")` |

# Documentation

The documentation website at <https://wlandau.github.io/crew/> includes
a [function
reference](https://wlandau.github.io/crew/reference/index.html) and
tutorial vignettes.

# Concepts

A *task* is a piece of R code, such as an expression or a function call.
A *worker* is a
[non-interactive](https://stat.ethz.ch/R-manual/R-devel/library/base/html/interactive.html)
R process that runs one or more tasks. When tasks run on workers, the
local R session is free and responsive, and work gets done faster. For
example, [this
vignette](https://wlandau.github.io/crew/articles/shiny.html) shows how
`crew` and [`mirai`](https://github.com/shikokuchuo/mirai) work together
to speed up [Shiny](https://rstudio.github.io/shiny/) apps.

# Plugins

A worker runs on your local computer, a SLURM cluster, AWS Batch, or any
number of other platforms. As long as the worker connects to your R
session over the local network, `crew` can use
[`mirai`](https://github.com/shikokuchuo/mirai) to send it tasks. You
can write your own [launcher
plugin](https://wlandau.github.io/crew/articles/plugins.html) to teach
`crew` how to use your platform. The
[`crew.cluster`](https://wlandau.github.io/crew.cluster/) package
supports plugins for SLURM, Sun Grid Engine (SGE), LSF, and PBS/TORQUE.

# Tasks

First, create a controller object to manage tasks and workers.

``` r
library(crew)
controller <- crew_controller_local(
  name = "example",
  workers = 2,
  seconds_idle = 10
)
```

Next, start the controller to create the
[`mirai`](https://github.com/shikokuchuo/mirai) client. Later, when you
are done with the controller, call `controller$terminate()` to clean up
the workers and dispatcher.

``` r
controller$start()
```

Use `push()` to submit a new task and `pop()` to return a completed
task.

``` r
controller$push(name = "get pid", command = ps::ps_pid())
```

Behind the scenes, `push()` and `pop()` also launch workers to run the
tasks. This process is expensive, so `crew` uses
[throttling](https://css-tricks.com/debouncing-throttling-explained-examples/).
That means not every call to `push()` or `pop()` launches any workers.
To ensure enough workers get launched, keep calling `pop()`.

``` r
controller$pop() # No workers started yet and the task is not done.
#> NULL

task <- controller$pop() # Worker started, task complete.
task
#> # A tibble: 1 × 11
#>   name    command result seconds   seed error trace warnings launcher worker
#>   <chr>   <chr>   <list>   <dbl>  <int> <chr> <chr> <chr>    <chr>     <int>
#> 1 get pid NA      <int>        0 1.78e9 NA    NA    NA       372d50b…      1
#> # ℹ 1 more variable: instance <chr>
```

Alternatively, `wait()` is a loop that repeatedly checks tasks and
launches workers until all tasks complete.

``` r
controller$wait(mode = "all")
```

The return value of the task is in the `result` column.

``` r
task$result[[1]] # return value of the task
#> [1] 69631
```

# Functional programming

The
[`map()`](https://wlandau.github.io/crew/reference/crew_class_controller.html#method-crew_class_controller-map)
method of the controller supports functional programming similar to
[`purrr::map()`](https://purrr.tidyverse.org/reference/map.html) and
[`clustermq::Q()`](https://mschubert.github.io/clustermq/reference/Q.html).
The arguments of
[`map()`](https://wlandau.github.io/crew/reference/crew_class_controller.html#method-crew_class_controller-map)
are mostly the same those of
[`push()`](https://wlandau.github.io/crew/reference/crew_class_controller.html#method-crew_class_controller-push),
but there is a new `iterate` argument to define the inputs of individual
tasks.
[`map()`](https://wlandau.github.io/crew/reference/crew_class_controller.html#method-crew_class_controller-map)
submits a whole collection of tasks, auto-scales the workers, waits for
all the tasks to finish, and returns the results in a `tibble`.

Below,
[`map()`](https://wlandau.github.io/crew/reference/crew_class_controller.html#method-crew_class_controller-map)
submits one task to compute `1 + 2 + 5 + 6` and another task to compute
`3 + 4 + 5 + 6`. The lists and vectors inside `iterate` vary from task
to task, while the elements of `data` and `globals` stay constant across
tasks.

``` r
results <- controller$map(
  command = a + b + c + d,
  iterate = list(
    a = c(1, 3),
    b = c(2, 4)
  ),
  data = list(c = 5),
  globals = list(d = 6)
)

results
#> # A tibble: 2 × 11
#>   name  command result    seconds      seed error trace warnings
#>   <chr> <chr>   <list>      <dbl>     <int> <chr> <chr> <chr>   
#> 1 1     NA      <dbl [1]>       0    1.82e9 NA    NA    NA      
#> 2 2     NA      <dbl [1]>       0    1.82e9 NA    NA    NA      
#> # ℹ 3 more variables: launcher <chr>, worker <int>,
#> #   instance <chr>

as.numeric(results$result)
#> [1] 14 18
```

# Summaries

The controller summary shows how many tasks each worker ran, how many
total seconds it spent running tasks, and how many tasks threw warnings
and errors.

``` r
controller$summary()
#> # A tibble: 2 × 6
#>   controller worker tasks seconds errors warnings
#>   <chr>       <int> <int>   <dbl>  <int>    <int>
#> 1 example         1     2   0.001      0        0
#> 2 example         2     1   0          0        0
```

The schedule summary counts “pushed” tasks which may not be complete and
“collected” tasks which `pop()` can return.

``` r
controller$schedule$summary()
#> # A tibble: 1 × 2
#>   pushed collected
#>    <int>     <int>
#> 1      0         0
```

The launcher summary counts the number of times each worker was
launched, and it shows the total number of assigned and completed tasks
from all past terminated instances of each worker.

``` r
controller$launcher$summary()
#> # A tibble: 2 × 4
#>   worker launches assigned complete
#>    <int>    <int>    <int>    <int>
#> 1      1        2        1        1
#> 2      2        1        0        0
```

Finally, the client summary shows up-to-date worker status from
`mirai::daemons()`.

``` r
controller$client$summary()
#> # A tibble: 2 × 6
#>   worker online instances assigned complete socket                          
#>    <int> <lgl>      <int>    <int>    <int> <chr>                           
#> 1      1 FALSE          1        2        2 ws://10.0.0.32:58685/1/15e07250…
#> 2      2 FALSE          1        1        1 ws://10.0.0.32:58685/2/cb45b3d4…
```

# Termination

Call `terminate()` on the controller after you finish using it.
`terminate()` tries to close the the
[`mirai`](https://github.com/shikokuchuo/mirai) dispatcher and any
workers that may still be running. It is important to free up these
resources.

``` r
controller$terminate()
```

# Scale

As explained above, `push()`, `pop()`, and `wait()` launch new workers
to run tasks. The number of new workers depends on the number of tasks
at the time. In addition, workers can shut themselves down as work
completes. In other words, `crew` automatically raises and lowers the
number of workers in response to fluctuations in the task workload.

The most useful arguments for down-scaling, in order of importance, are:

1.  `seconds_idle`: shut down a worker if it spends too long waiting for
    a task.
2.  `tasks_max`: shut down a worker after it completes a certain number
    of tasks.
3.  `seconds_wall`: soft wall time of a worker.

Please tune these these arguments to achieve the desired balance for
auto-scaling. The two extremes of auto-scaling are
[`clustermq`](https://mschubert.github.io/clustermq/)-like *persistent
workers* and [`future`](https://future.futureverse.org/)-like *transient
workers*, and each is problematic in its own way.

1.  *Persistent workers*: a persistent worker launches once, typically
    runs many tasks, and stays running for the entire lifetime of the
    controller. Persistent workers minimize overhead and quickly
    complete large numbers of short tasks. However, they risk spending
    too much time in an idle state if there are no tasks to run.
    Excessive idling wastes resources, which could impact your
    colleagues on a shared cluster or drive up costs on Amazon Web
    Services.
2.  *Transient workers*: a transient worker terminates as soon as it
    completes a single task. Each subsequent task requires a new
    transient worker to run it. Transient workers avoid excessive
    idling, but frequent worker launches cause significant overhead and
    slows down the computation as a whole.

# Risks

The `crew` package has unavoidable risk. It is your responsibility as
the user to safely use `crew`. Please read the final clause of the
[software license](https://wlandau.github.io/crew/LICENSE.html).

## Security

`crew` currently uses unencrypted TCP connections for transactions with
workers inside a trusted local network. In a compromised network, an
attacker can potentially access and exploit sensitive resources. It is
your responsibility to assess the sensitivity and vulnerabilities of
your computing environment and make sure your network is secure.

## Ports

`crew` uses one TCP port per controller. TCP ports range from 0 to
65535, and only around 16000 of these ports are considered ephemeral or
dynamic, so please be careful not to run too many controllers
simultaneously if you are running R on a machine you share with other
people (such as the login node of a shared cluster). If you are running
a [controller
group](https://wlandau.github.io/crew/articles/groups.html) please add
only a small number of controllers to the group. The `terminate()` frees
these ports again for other processes to use.

## Zombies

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

`mirai` robustly terminates workers as appropriate, but this safeguard
cannot protect against the risk of a worker that gets stuck in a crashed
state before it can even start R. To be absolutely sure that workers do
not run indefinitely if something goes wrong, please learn how to find
and terminate workers on the specific computing platform where they run.
And if you are writing a custom launcher plugin, it is recommended
(although not strictly required) to write a custom `terminate_worker()`
method.

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

### Dispatcher

The [`mirai`](https://github.com/shikokuchuo/mirai) dispatcher is
designed to gracefully exit when you call `terminate()` on the
controller object or when you restart your R session. However, if you
ever need to shut down the dispatcher manually, you can find the process
ID using the controller object, then use `ps::ps_kill()` to terminate
the process.

``` r
controller$client$dispatcher
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
    [`mirai`](https://github.com/shikokuchuo/mirai) to different
    computing platforms for distributed workers.
  - [`rrq`](https://mrc-ide.github.io/rrq/): a task queue for R based on
    [Redis](https://redis.io).
  - [`rrqueue`](http://traitecoevo.github.io/rrqueue/): predecessor of
    [`rrq`](https://mrc-ide.github.io/rrq/).
  - [`clustermq`](https://mschubert.github.io/clustermq/): sends R
    function calls as jobs to computing clusters.
  - [`future`](https://future.futureverse.org/): a unified interface for
    asynchronous evaluation of single tasks and map-reduce calls on a
    wide variety of backend technologies.
  - [`batchtools`](https://mllg.github.io/batchtools/): tools for
    computation on batch systems.
  - [`targets`](https://docs.ropensci.org/targets/): a Make-like
    pipeline tool for R.
  - [`later`](https://r-lib.github.io/later/): delayed evaluation of
    synchronous tasks.
  - [`promises`](https://rstudio.github.io/promises/):
    minimally-invasive asynchronous programming for a small number of
    tasks within Shiny apps.
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
    accommodated the complicated and demanding feature requests that
    made `crew` possible.
  - [Rich FitzJohn](https://github.com/richfitz) and [Robert
    Ashton](https://github.com/r-ash) developed
    [`rrq`](https://mrc-ide.github.io/rrq//).
  - [Gábor Csárdi](https://github.com/gaborcsardi/) developed
    [`callr`](https://github.com/r-lib/callr) and wrote an [edifying
    blog post on implementing task
    queues](https://www.tidyverse.org/blog/2019/09/callr-task-q/).
  - [Kirill Müller](https://github.com/krlmlr/) created the
    [`workers`](https://github.com/wlandau/workers) prototype, an
    initial effort that led directly to the current implementation of
    `crew`. `crew` would not exist without Kirill’s insights about
    orchestration models for R processes.
  - [Henrik Bengtsson](https://github.com/HenrikBengtsson/). Henrik’s
    [`future`](https://github.com/HenrikBengtsson/future/) package
    ecosystem demonstrates the incredible power of a consistent R
    interface on top of a varying collection of high-performance
    computing technologies.
  - [Michael Schubert](https://github.com/mschubert/). Michael’s
    [`clustermq`](https://mschubert.github.io/clustermq/) package
    supports efficient high-performance computing on traditional
    clusters, and it demonstrates the value of a central `R6` object to
    manage an entire collection of persistent workers.
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

# Code of Conduct

Please note that the `crew` project is released with a [Contributor Code
of
Conduct](https://github.com/wlandau/crew/blob/main/CODE_OF_CONDUCT.md).
By contributing to this project, you agree to abide by its terms.

# Citation

``` r
To cite package ‘crew’ in publications use:

  Landau WM (2023). _crew: A Distributed Worker Launcher Framework_.
  https://wlandau.github.io/crew/, https://github.com/wlandau/crew.

A BibTeX entry for LaTeX users is

  @Manual{,
    title = {crew: A Distributed Worker Launcher Framework},
    author = {William Michael Landau},
    year = {2023},
    note = {https://wlandau.github.io/crew/, https://github.com/wlandau/crew},
  }
```
