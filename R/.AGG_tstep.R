## choose a timestep for aggregation

.AGG_tstep <- function(DateTimes, chunks, lock.ts) {

  ###### DEFAULTS ####

  if (missing(lock.ts)){
    lock.ts <- FALSE
  }

  if (missing(chunks)){
    chunks <- 2000
  }

  # set system to UTC
  Sys.setenv(tz = 'UTC')

  #### END DEFAULTS ####

  #### MAIN FUNCTION ####

  dateStart  <- min(DateTimes)
  dateEnd    <- max(DateTimes)
  dateSpan   <- as.numeric(dateEnd - dateStart) * 86400

  # find aggregation timestep in seconds for the number of chunks specified
  timeRes <- round(dateSpan / chunks, 0)

  if (lock.ts == TRUE) {

    # aggergation windows, in seconds (from 1 min to 1 wk)
    intervals <- c(60, 60*15, 3600, 3600*3, 3600*6, 3600*12, 86400, 86400*7)
    # identify closest window
    nearest <- findInterval(timeRes,intervals)

    # this will be the new timewindow
    timestep <- intervals[nearest]

      }

  return(timestep)

}
