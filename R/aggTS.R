#' Aggregate timeseries - low level function
#'
#' @export
#' @param dt_in data table object
#' @param timestep new timestep used in the aggregation results in sec
#' @param FUN aggregation method; mean, median, sum, min, max, or circular (for averaging direction measurements in degrees)
#' @param pullAgg aggregate data from before/on new timestamp ('left'; default), either side of timestamp ('centre'), or on/after timestamp ('right')
#' @keywords wrangling
#' @examples LF = aggTS(dataIn = myDF, timestep = 60x60x4, FUN = "mean", pullAgg = "center", outType = "LF")
#'
#'

aggTS <- function(dt_in, timestep, FUN, pullAgg){    # , outType

  ######## set defaults ########
  tz.src = Sys.timezone()  # back up
  Sys.setenv(tz = 'UTC')

  if (missing(timestep)){
    timestep <- 60*60*24
  }

  if (missing(FUN)){
    FUN <- "mean"
  }

  if (missing(pullAgg)){
    pullAgg <- "left"
  }

  ######## end defaults ########

  ### define aggregation function
  if (!is.function(FUN)){
    if (FUN == "mean"){
      FUN <- function(d){mean(d, na.rm = TRUE)}

    } else if (FUN == "max"){
      FUN <- function(d){max(d, na.rm = TRUE)}

    } else if (FUN == "min"){
      FUN <- function(d){min(d, na.rm = TRUE)}

    } else if (FUN == "var"){
      FUN <- function(d){var(d, na.rm = TRUE)}

    } else if (FUN == "sd"){
      FUN <- function(d){sd(d, na.rm = TRUE)}

    } else if (FUN == "median"){
      FUN <- function(d){median(d, na.rm = TRUE)}

    } else if (FUN == "sum"){
      FUN <- function(d){sum(d, na.m = TRUE)}

    } else if (FUN == "count"){
      FUN <- function(d){sum(!is.na(d))}

    } else if (FUN == "circular"){
      FUN <- function(d){mean(circular::circular(d, units = "degrees", modulo = '2pi'))}

    } else if (substr(FUN,1,1) == "p"){
      FUN <- function(d){quantile(d, probs = as.numeric(substr(FUN,2,3)) / 100, na.rm = TRUE)}

    } # end common aggregation function match

  } # end user function check

  ## end function definition ##

  ### make timestamp series based on desired pull direction
  if (pullAgg == "left") {

    newTS =
      as.numeric(
        lapply(
          as.numeric(dt_in$DateTime) / timestep,
          floor
        )
      ) * timestep

  } else if (pullAgg == "right") {

    newTS =
      as.numeric(
        lapply(
          as.numeric(dt_in$DateTime) / timestep,
          ceiling
        )
      ) * timestep

  } else {
    newTS =
      as.numeric(
        lapply(
          as.numeric(dt_in$DateTime) / timestep,
          round
        )
      ) * timestep

  }


  ### make the new data table for aggregated data
  dt_agg <- dt_in
  # overwrite with modified timestamps
  dt_agg$DateTime <- as.POSIXct(newTS,
                                origin = '1970-01-01 00:00.00 UTC',
                                tz = "UTC"
  )

  ### do the aggregation ###
  dt_agg <- dt_agg[,lapply(.SD, FUN), by = 'DateTime']

  ## return values
  return(dt_agg)

}

