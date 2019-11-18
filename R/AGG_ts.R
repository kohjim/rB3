#' Aggregate a timeseries - low level function. REturns a list of new DateTImes and aggregated values
#'
#' @export
#' @param values a vector of numeric values
#' @param DateTimes a vector of POSIXct timestamps corresponding to values vector
#' @param FUN aggregation method (chr)
#' @param pullAgg aggregate data from before/on new timestamp ('left'; default), either side of timestamp ('centre'), or on/after timestamp ('right')
#' @keywords wrangling
#' @examples aggTS = .AGG_ts(values, DateTimes, timestep = 60x60x4, FUN = "mean", pullAgg = "center")
#'
#'

.AGG_ts <- function(values, DateTimes, timestep, FUN, pullAgg){    # , outType

  ######## set defaults ########
  tz.src = Sys.timezone()  # back up
  Sys.setenv(tz = 'UTC')

  if (missing(timestep)){
    timestep <- 60*60*24       # 1 day default
  }

  if (missing(FUN)){
    FUN <- "mean"
  }

  if (missing(pullAgg)){
    pullAgg <- "left"
  }

  ######## end defaults ########


  ## regularise the dates vector
  newDates <- .AGG_regDates(DateTimes, timestep, pullAgg = pullAgg)



  ### define aggregation function

  if (!is.function(FUN)){
    if (FUNlist[i] == "mean"){
      FUN <- function(d){mean(d, na.rm = TRUE)}

    } else if (FUNlist[i] == "max"){
      FUN <- function(d){max(d, na.rm = TRUE)}

    } else if (FUNlist[i] == "min"){
      FUN <- function(d){min(d, na.rm = TRUE)}

    } else if (FUNlist[i] == "var"){
      FUN <- function(d){var(d, na.rm = TRUE)}

    } else if (FUNlist[i] == "sd"){
      FUN <- function(d){sd(d, na.rm = TRUE)}

    } else if (FUNlist[i] == "median"){
      FUN <- function(d){median(d, na.rm = TRUE)}

    } else if (FUNlist[i] == "sum"){
      FUN <- function(d){sum(d, na.rm = TRUE)}

    } else if (FUNlist[i] == "count"){
      FUN <- function(d){sum(!is.na(d))}

    } else if (FUNlist[i] == "circular"){
      FUN <- function(d){mean(circular::circular(d, units = "degrees", modulo = '2pi'))}

    } else if (substr(FUNlist[i],1,1) == "p"){
      FUN <- function(d){quantile(d, probs = as.numeric(substr(FUN,2,3)) / 100, na.rm = TRUE)}

    } # end common aggregation function match

  } # end user function check


  # force inputs to a data.table
  dt.agg <- data.table::data.table(newDates, values)

  ## aggregate it
  dt.agg <- dt.agg[,lapply(.SD, FUN), by = 'newDates']

  # set non-numerics back to NA
  dt.agg[!is.finite(dt.agg$values),'values'] <- NA

  ### a list to return
  outs <- list(dt.agg$newDates, dt.agg$values)
    names(outs) <- c("DateTime","values")

  ## return values
  return(dt.agg)

}

