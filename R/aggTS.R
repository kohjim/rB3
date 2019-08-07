#' Aggregate timeseries - low level function
#'
#' Trim dates and/or aggregate data frame
#'
#' @export
#' @param dataIn data frame object
#' @param timestep new timestep used in the aggregation results in sec
#' @param FUN aggregation method; mean, median, sum, min, max, or circular (for averaging direction measurements in degrees)
#' @param pullAgg aggregate data from before/on new timestamp ('left'; default), either side of timestamp ('centre'), or on/after timestamp ('right')
#' @param outType output data. "LF" for low resolution data frame, "HF" for original resolution data with bin, and "both" for both formats in the list
#' @keywords wrangling
#' @examples LF = aggTS(dataIn = myDF, timestep = 60x60x4, FUN = "mean", pullAgg = "center", outType = "LF")
#'
#'

aggTS <- function(dataIn, timestep, FUN, pullAgg, outType){

  ######## set defaults ########

  tz.src = Sys.timezone()  # back up
  Sys.setenv(tz = 'UTC')

  DF_HF = dataIn
  if (ncol(DF_HF) > 2){
    multiVars = 1
  } else {
    multiVars = 0
  }

  if (missing(timestep)){
    timestep <- 60*60*24
  }

  if (missing(FUN)){
    FUN <- "mean"
  }

  if (missing(pullAgg)){
    pullAgg <- "left"
  }

  if (missing(outType)){
    pullAgg <- "LF"
  }

  ######## end defaults ########
  ## ts conversion rate
  ts_convRate = timestep # sec

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

  if (!multiVars){
    colnames(DF_HF) = c("DateTime","Data","newTS")
  } else {
    colnames(DF_HF) = c("DateTime","var","Data","newTS")
  }

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

  if (!multiVars){
    DF_LF = DF_HF[
      ,
      list(agg = FUN(Data)),
      by = list(newTS)
      ]

  } else {
    DF_LF = DF_HF[
      ,
      list(agg = FUN(Data)),
      by = 'newTS,var'
      ]
  }

  DF_LF$agg[is.infinite(DF_LF$agg)] = NA

  ## return values

  Sys.setenv(tz = tz.src)

  if (outType == "LF"){
    return(DF_LF)

  } else if (outType == "HF") {
    return(DF_HF)

  } else {
    return(list(DF_HF,DF_LF))

  }
}
