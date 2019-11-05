## make vector of regular timestamps for a data frame (to pass to aggregation)

zWRG_reg_dates <- function(dt.in, timestep, pullAgg) {

  # default
  if (missing(pullAgg)){
    pullAgg <- 'left'
  }


### make timestamp series based on desired pull direction
if (pullAgg == "left") {

  newTS =
    as.numeric(
      lapply(
        as.numeric(dt.in$DateTime) / timestep,
        floor
      )
    ) * timestep

} else if (pullAgg == "right") {

  newTS =
    as.numeric(
      lapply(
        as.numeric(dt.in$DateTime) / timestep,
        ceiling
      )
    ) * timestep

} else {
  newTS =
    as.numeric(
      lapply(
        as.numeric(dt.in$DateTime) / timestep,
        round
      )
    ) * timestep

  }

  # force to POSIX
  newTS <- as.POSIXct(newTS,
                      origin = "1970-01-01 00:00:00",
                      format = "%Y-%m-%d %H:%M:%S",
                      tz = "UTC")

  return(newTS)

}
