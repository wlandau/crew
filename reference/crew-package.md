# crew: a distributed worker launcher framework

In computationally demanding analysis projects, statisticians and data
scientists asynchronously deploy long-running tasks to distributed
systems, ranging from traditional clusters to cloud services. The
[NNG](https://nng.nanomsg.org)-powered
[`mirai`](https://github.com/r-lib/mirai) R package is a sleek and
sophisticated scheduler that efficiently processes these intense
workloads. The `crew` package extends
[`mirai`](https://github.com/r-lib/mirai) with a unifying interface for
third-party worker launchers. Inspiration also comes from packages
[`future`](https://future.futureverse.org/),
[`rrq`](https://mrc-ide.github.io/rrq/),
[`clustermq`](https://mschubert.github.io/clustermq/), and
[`batchtools`](https://github.com/mlr-org/batchtools).
