## choose a timestep for aggregation

zWRG_agg_tstep <- function(dt.in, chunks, lock.ts) {

  ###### DEFAULTS ####

  if (missing(lock.ts)){
    lock.ts <- FALSE
  }

  if (missing(chunks)){
    chunks <- 2000
  }

  # set system to UTC
  Sys.setenv(tz = 'UTC')

  # force to correct formats, in case
  dt.in <- data.table(dt.in)

  dt.in$DateTime <- as.POSIXct(dt.in$DateTime,
                               origin = "1970-01-01 00:00:00",
                               format = "%Y-%m-%d %H:%M:%S",
                               tz = "UTC")

  #### END DEFAULTS ####

  #### MAIN FUNCTION ####

  dateStart  <- min(dt.in$DateTime)
  dateEnd    <- max(dt.in$DateTime)
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
