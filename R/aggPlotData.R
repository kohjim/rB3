#' Aggregate a dataset for plotting large time-series quickly
#' Wide format in, long format out
#'
#' @export
#' @param dt_in data in
#' @keywords plotting
#' @examples aggPlotData(dt_in = myDT)
#'

aggPlotData <- function(dt_in) {  # ,varNames,timeUnit

  ######## set defaults ########

  # tz.src = Sys.timezone()  # back up
    Sys.setenv(tz = 'UTC')

  ######## end defaults ########


  ######## main function ########

  ## get relevant attributes for the data
  varNames <- names(dt_in[,2:ncol(dt_in)])

  # only aggregate big data
  if(nrow(dt_in) < 50000) {

    return(dt_in)

  } else {


  dateStart  <- min(dt_in$DateTime)
  dateEnd    <- max(dt_in$DateTime)
    dateSpan   <- as.numeric(dateEnd - dateStart) * 86400

  ## determine the best time res for efficient plotting
    # aim for max 2000 pts to render per time-series, round to logical aggregation intervals
    timeRes <- dateSpan / 2480
    # aggergation windows, in seconds (from 1 min to 1 wk)
    intervals <- c(60, 60*15, 3600, 3600*3, 3600*6, 3600*12, 86400, 86400*7)
    # identify closest window
    nearest <- findInterval(timeRes,intervals)

      # this will be the new timewindow
      timeRes <- intervals[nearest]


  # make all possible dates at the aggregated timestep, to use later to ensure missing values are rendered
  start <- plyr::round_any(dateStart - timeRes, accuracy = timeRes, f = round)
  end   <- plyr::round_any(dateEnd, accuracy = timeRes, f = round)
    dates <- seq.POSIXt(start,end,timeRes)
   nas <- data.table(dates,rep(NA,length(dates)))
       names(nas) <- c("DateTime","value")

  ## floor the timestamp to the nearest res
  dt_in$DateTime <- plyr::round_any(dt_in$DateTime, accuracy = timeRes, f = round)

  # function to aggregate one var, using data table
  aggTS <- function(varName) {

    # trim to the varName of interest, as data table
    stats <- dt_in[!is.na(dt_in[,get(varName)]),mget(c("DateTime",varName))]

    # find stats, bookend with means so that lines render to the means rather than extremes
    stats <- setDT(stats)[,
                          .(mean1 = mean(get(varName)), na.rm = T,
                            min = min(get(varName)), na.rm = T,
                            max = max(get(varName)), na.rm = T,
                            mean2 = mean(get(varName)), na.rm = T),
                          by = 'DateTime']

    # convert to long
    stats = melt(stats, id.vars = c("DateTime"),
                 measure.vars = c("mean1","min", "max","mean2"))

    # only keep those nas for when we don't have any data at all (per aggregation timestep)
    nasVar <- nas[!nas$DateTime %in% unique(stats$DateTime),]

    # convert to data frame for ggplot
    aggd <- as.data.frame(rbind(nasVar, stats[,c(1,3)]))
    # add variable name for long merge
    aggd$var <- varName
    aggd <- aggd[order(aggd$DateTime),]

    return(aggd)
  }

  # loop over all vars needed
  df_out <- lapply(varNames, function(x) aggTS(varName = x))

  #combine into one DF
  df_out <- do.call("rbind", df_out)

  df_out <- df_out[,c("DateTime","var","value")]

  return(df_out)

  }
}

######## main function ########


