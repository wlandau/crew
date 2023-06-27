library(crew)
controller <- crew_controller_local(workers = 20)
controller$start()
system.time(
  out <- controller$map(
    command = if (x <= 2000L) {
      stop(x) 
    } else if (x <= 10000L) {
      warning(x)
    },
    iterate = list(x = seq_len(60000L)),
    error = "silent"
  )
)

controller$terminate()
