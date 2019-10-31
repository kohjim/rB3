#' Aggregate a data frame for plotting large time-series quickly
#'
#' @export
#' @param df_in data frame object
#' @param varNames aggregation method; mean, median, sum, min, max, or circular (for averaging direction measurements in degrees)
#' @param timeUnit aggregation window, e.g. 'days', '6 hours, 'hours'
#' @keywords plotting
#' @examples plotDF = aggPlotDF(dataIn = myDF, varNames = "TmpWtr", timeUnit = 'days')
#'

aggPlotData <- function(df_in,varNames,timeUnit) {

  ######## set defaults ########

  # tz.src = Sys.timezone()  # back up
  # Sys.setenv(tz = 'UTC')

  if (missing(varNames)){
    varNames <- colnames(df_in[,2:ncol(df_in)])
  }

  if (missing(timeUnit)){
    timestep <- 'days'
  }

  ######## main function ########

  # force to df (in case it's a data.table)
  df_in <- data.frame(df_in)
  # floor the date
  df_in$DateTime <- lubridate::floor_date(df_in$DateTime, timeUnit)

  # make all possible dates at the aggregated timestep, to use later to ensure missing values are rendered
  dates <- seq.POSIXt(min(df_in$DateTime),max(df_in$DateTime),timeUnit)
  nas <- data.frame(dates,rep(NA,length(dates)))
     names(nas) <- c("DateTime","value")

  # function to aggregate one var, using data table
  comberoo <- function(varName) {

    # trim to the varName of interest, as data table
    stats <- data.table(df_in[!is.na(df_in[,varName]),c("DateTime",varName)])
    # find stats, bookend with means so that lines render to the means rather than extremes
    stats <- setDT(stats)[,.(mean = mean(get(varName)), min = min(get(varName)), max = max(get(varName)), mean = mean(get(varName))), by = 'DateTime']
    # convert to long
    stats = melt(stats, id.vars = c("DateTime"),
                 measure.vars = c("min", "max","mean"))

    # only keep those nas for when we don't have any data at all (per aggregation timestep)
    nasVar <- nas[!nas$DateTime %in% unique(stats$DateTime),]

    # convert to df for ggplot
    aggd <- as.data.frame(rbind(nasVar, stats[,c(1,3)]))
    # add variable name for long merge
    aggd$var <- varName
    aggd <- aggd[order(aggd$DateTime),]

    return(aggd)
  }

  # loop over all vars needed
  df_out <- lapply(varNames, function(x) comberoo(varName = x))
  #combine into one DF
  df_out <- do.call("rbind", df_out)

  return(df_out)
}

######## main function ########


