#' Aggregate timeseries - low level function
#'
#' Trim dates and/or aggregate data frame
#'
#' @export
#' @param dataIn data frame object
#' @param timestep standardise (aggregate) to a common timestep (numeric, in mins, to turn aggregation on) !!
#' @param FUN aggregation method; mean, median, sum, min, max, or circular (for averaging direction measurements in degrees)
#' @param pullAgg aggregate data from before/on new timestamp ('left'; default), either side of timestamp ('centre'), or on/after timestamp ('right')
#' @keywords fileIO
#' @examples stdDF <- rB3stdze(rB3in = rawDF, varNames = wqVars, metaD = metaD, startDate = '2016-07-01 00:00:00', endDate = '2018-06-30 23:45:00', doAgg = TRUE, methodAgg = ctrls$methodAgg, pullAgg = ctrls$pullAgg)
#'
#'

aggTS <- function(dataIn, timestep, FUN, pullAgg){

  ######## set defaults ########

  tz.src = Sys.timezone()  # back up
  Sys.setenv(tz = 'UTC')

  if (is.data.frame(dataIn)){
    # in case input was a data frame
    DF_HF <- dataIn

  } else {
    # otherwise it has to be an rB3 object
    DF_HF <- dataIn[["qcDF"]]
  }

  if (missing(timestep)){
    timestep <- 60*24
  }

  if (missing(FUN)){
    FUN <- "mean"
  }

  if (missing(pullAgg)){
    pullAgg <- "left"
  }

  ######## end defaults ########
  ## ts conversion rate
  ts_convRate = timestep * 60 # sec

  ## making aggregation TS index
  if (pullAgg == "left") {

    newTS =
      as.numeric(
        lapply(
          as.numeric(as.POSIXct(DF_HF[,1])) / ts_convRate,
          floor
        )
      ) * ts_convRate

  } else if (pullAgg == "right") {

    newTS =
      as.numeric(
        lapply(
          as.numeric(as.POSIXct(DF_HF[,1])) / ts_convRate,
          ceiling
        )
      ) * ts_convRate

  } else {
    newTS =
      as.numeric(
        lapply(
          as.numeric(as.POSIXct(DF_HF[,1])) / ts_convRate,
          round
        )
      ) * ts_convRate

  }

  DF_HF = cbind(DF_HF, newTS)
  colnames(DF_HF) = c("DateTime","Data","newTS")

  DF_HF$newTS = as.POSIXct(
    DF_HF$newTS,
    origin = '1970-01-01 00:00.00 UTC'
  )

  ## define aggregation function
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

    } else if (FUN == "count"){
      FUN <- function(d){sum(!is.na(d))}

    } else if (substr(FUN,1,1) == "p"){
      FUN <- function(d){quantile(d, probs = as.numeric(substr(FUN,2,3)) / 100, na.rm = TRUE)}

    } # end common aggregation function match

  } # end user function check

  ## end function definition ##

  # setDT(DF_HF) # this doesn't work in R function ... convert them manually as below
  DF_HF = data.table(DF_HF)

  DF_LF = DF_HF[
    ,
    list(agg = FUN(Data)),
    by = list(newTS)
    ]

  Sys.setenv(tz = tz.src)

  return(DF_LF)
}
