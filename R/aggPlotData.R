#' Aggregate a dataset for plotting large time-series quickly
#' Wide format in, long format out
#'
#' @export
#' @param dt.in data in (data.frame?)
#' @keywords plotting
#' @examples aggPlotData(dt.in = myDT)
#'

aggPlotData <- function(dt.in) {  # ,varNames,timeUnit

  ######## set defaults ########

  # tz.src = Sys.timezone()  # back up
  Sys.setenv(tz = 'UTC')

  # force to correct formats
  dt.in <- data.table(dt.in)

  dt.in$DateTime <- as.POSIXct(dt.in$DateTime,
                               origin = "1970-01-01 00:00:00",
                               format = "%Y-%m-%d %H:%M:%S",
                               tz = "UTC")

  ######## END DEFAULTS ########


  ######## MAIN FUNCTION ########

  ## get relevant attributes for the data
  varNames <- names(dt.in[,2:ncol(dt.in)])


  ### only aggregate big data
  if(nrow(dt.in) < 50000) {

   dt.out <- tidyr::gather(dt.in,var,value,2:ncol(dt.in))

  } else {

  ### calculate the timestep (locked to sensible intervals)
  timeRes <- zWRG_agg_tstep(dt.in, chunks = 1500, lock.ts = TRUE)

  # make all possible dates at the aggregated timestep
  aggDates <- zWRG_reg_dates(dt.in, timestep = timeRes, pullAgg = 'left')

  # unique aggregate timesteps
  aggUnique <- unique(aggDates)


  # create df for aggregating
  dt.agg <- dt.in

      # aggregate the date stamps
      dt.agg$DateTime <- aggDates


  #### aggregate the dataframe by mean, min and max to use later
  funs <- c('mean','min','max')

  # apply all functions to the data using aggTS
  aggd <- lapply(funs, aggTS, dt.in = dt.agg, timestep = timeRes, pullAgg = 'left')

  ### bookend the min max with means for each aggregate timestep
  aggd <- rbind(aggd[[1]], aggd[[2]], aggd[[3]], aggd[[1]] )

  # long format
  dt.out <- tidyr::gather(aggd, var,value,2:ncol(aggd))
  # remove non-numeric
  dt.out <- dt.out[!is.infinite(dt.out$value),]
  dt.out <- dt.out[!is.na(dt.out$value),]


  ### make a dt of agg dates and NA, for use later to make sure NAs are rendered.

  nas <- data.table(aggUnique,
                    rep(varNames[1],length(aggUnique)),
                    rep(NA,length(aggUnique))
                    )
       names(nas) <- c("DateTime","var","value")

  # only keep those NAs for when we don't have any data at all (per aggregation timestep)
  nas <- nas[!nas$DateTime %in% dt.out$DateTime,]

  # convert to data frame for ggplot
  dt.out <- rbind(nas, dt.out)

  # sort to correct order
  dt.out <- dt.out[order(dt.out$DateTime),]


  }



  return(dt.out)

}


####### END FUNCTION ########


