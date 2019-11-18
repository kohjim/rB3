## transform a vector of POSIXct DateTimes into regular timestep (keep same length), and define direction of ts 'pull'

#' @param dates vector of POSIXct
#' @param timestep in seconds
#' @param pullAgg direction in which to pull timesteps


.AGG_regDates <- function(DateTimes, timestep, pullAgg) {

  # default
  if (missing(pullAgg)){
    pullAgg <- 'left'
  }


### make timestamp series based on desired pull direction
if (pullAgg == "left") {

  newTS = floor(as.numeric(DateTimes)/ timestep) * timestep

 } else if (pullAgg == "right") {

  newTS = floor(as.numeric(DateTimes) / timestep) * timestep

 } else {

  newTS = round(as.numeric(DateTimes) / timestep) * timestep

 }

  # force to POSIX
  newTS <- as.POSIXct(newTS,
                      origin = "1970-01-01 00:00:00",
                      format = "%Y-%m-%d %H:%M:%S",
                      tz = "UTC")

  return(newTS)

}
