
# crew: a distributed worker launcher framework <img src='man/figures/logo-readme.png' align="right" height="139"/>

<!-- badges: start --->

[![CRAN](https://www.r-pkg.org/badges/version/crew)](https://CRAN.R-project.org/package=crew)
[![status](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![check](https://github.com/wlandau/crew/actions/workflows/check.yaml/badge.svg)](https://github.com/wlandau/crew/actions?query=workflow%3Acheck)
[![codecov](https://codecov.io/gh/wlandau/crew/branch/main/graph/badge.svg?token=3T5DlLwUVl)](https://app.codecov.io/gh/wlandau/crew)
[![lint](https://github.com/wlandau/crew/actions/workflows/lint.yaml/badge.svg)](https://github.com/wlandau/crew/actions?query=workflow%3Alint)
[![pkgdown](https://github.com/wlandau/crew/actions/workflows/pkgdown.yaml/badge.svg)](https://github.com/wlandau/crew/actions?query=workflow%3Apkgdown)
<!-- badges: end --->

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
extend `crew` to modern platforms and services like [Google Cloud
Run](https://cloud.google.com/run),
[Kubernetes](https://kubernetes.io/), and beyond. At the time of
writing, plugins for cloud computing do not yet exist. So if you have
access to these services and know how to use them, please consider
contributing a package with plugins of your own. The tutorial at
<https://wlandau.github.io/crew/articles/plugins.html> explains how. The
maintainer of `crew` would love to work with you!**

# Installation

| Type | Source | Command |
|----|----|----|
| Release | CRAN | `install.packages("crew")` |
| Development | GitHub | `remotes::install_github("wlandau/crew")` |
| Development | R-universe | `install.packages("crew", repos = "https://wlandau.r-universe.dev")` |

# Documentation

The documentation website at <https://wlandau.github.io/crew/> includes
a [function
reference](https://wlandau.github.io/crew/reference/index.html) and
tutorial vignettes linked below.

1.  [Introduction to
    `crew`](https://wlandau.github.io/crew/articles/introduction.html)
2.  [Controller
    groups](https://wlandau.github.io/crew/articles/groups.html)
3.  [Integration with
    Shiny](https://wlandau.github.io/crew/articles/shiny.html)
4.  [How to write launcher
    plugins](https://wlandau.github.io/crew/articles/plugins.html)
5.  [Known risks of
    `crew`](https://wlandau.github.io/crew/articles/risks.html)

# Risks

The `crew` package has unavoidable risks, and the user is responsible
for safety, security, and computational resources. Please read the
[software license](https://wlandau.github.io/crew/LICENSE.html) and the
[vignette about specific known
risks](https://wlandau.github.io/crew/articles/risks.html).

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
- [Henrik Bengtsson](https://github.com/futureverse/). Henrik’s
  [`future`](https://github.com/futureverse/future/) package ecosystem
  demonstrates the incredible power of a consistent R interface on top
  of a varying collection of high-performance computing technologies.
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
- [Joe Cheng](https://github.com/jcheng5) for sparking the integration
  of `crew` with [`promises`](https://rstudio.github.io/promises/).

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
