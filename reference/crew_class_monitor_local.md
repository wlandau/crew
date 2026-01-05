# Local monitor class

Local monitor `R6` class

## Details

See
[`crew_monitor_local()`](https://wlandau.github.io/crew/reference/crew_monitor_local.md).

## See also

Other monitor:
[`crew_monitor_local()`](https://wlandau.github.io/crew/reference/crew_monitor_local.md)

## Methods

### Public methods

- [`crew_class_monitor_local$dispatchers()`](#method-crew_class_monitor_local-dispatchers)

- [`crew_class_monitor_local$daemons()`](#method-crew_class_monitor_local-daemons)

- [`crew_class_monitor_local$workers()`](#method-crew_class_monitor_local-workers)

- [`crew_class_monitor_local$terminate()`](#method-crew_class_monitor_local-terminate)

------------------------------------------------------------------------

### Method `dispatchers()`

List the process IDs of the running `mirai` dispatcher processes.

#### Usage

    crew_class_monitor_local$dispatchers(user = ps::ps_username())

#### Arguments

- `user`:

  Character of length 1, user ID to filter on. `NULL` to list processes
  of all users (not recommended).

#### Returns

Integer vector of process IDs of the running `mirai` dispatcher
processes.

------------------------------------------------------------------------

### Method `daemons()`

List the process IDs of the locally running `mirai` daemon processes
which are not `crew` workers.

#### Usage

    crew_class_monitor_local$daemons(user = ps::ps_username())

#### Arguments

- `user`:

  Character of length 1, user ID to filter on. `NULL` to list processes
  of all users (not recommended).

#### Returns

Integer vector of process IDs of the locally running `mirai` daemon
processes which are not `crew` workers.

------------------------------------------------------------------------

### Method `workers()`

List the process IDs of locally running `crew` workers launched by the
local controller
([`crew_controller_local()`](https://wlandau.github.io/crew/reference/crew_controller_local.md)).

#### Usage

    crew_class_monitor_local$workers(user = ps::ps_username())

#### Arguments

- `user`:

  Character of length 1, user ID to filter on. `NULL` to list processes
  of all users (not recommended).

#### Details

Only the workers running on your local computer are listed. Workers that
are not listed include jobs on job schedulers like SLURM or jobs on
cloud services like AWS Batch. To monitor those worker processes, please
consult the monitor objects in the relevant third-party launcher plugins
such as `crew.cluster` and `crew.aws.batch`.

#### Returns

Integer vector of process IDs of locally running `crew` workers launched
by the local controller
([`crew_controller_local()`](https://wlandau.github.io/crew/reference/crew_controller_local.md)).

------------------------------------------------------------------------

### Method `terminate()`

Terminate the given process IDs.

#### Usage

    crew_class_monitor_local$terminate(pids)

#### Arguments

- `pids`:

  Integer vector of process IDs of local processes to terminate.

#### Details

Termination happens with the operating system signal given by
[`crew_terminate_signal()`](https://wlandau.github.io/crew/reference/crew_terminate_signal.md).

#### Returns

`NULL` (invisibly).
