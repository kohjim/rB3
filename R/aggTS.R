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


agg.ts <- function(dataIn, timestep, FUN, pullAgg){
  
  ######## set defaults ########
  
  if (is.data.frame(dataIn)){
    # in case input was a data frame
    DF_HF <- dataIn

  } else {
    # otherwise it has to be an rB3 object
    DF_HF <- dataIn[["qcDF"]]
  }
  
  if (missing(timestep)){
    timestep <- metaD$timestep
  }
  
  if (missing(FUN)){
    FUN <- "mean"
  }
  
  if (missing(pullAgg)){
    pullAgg <- "left"
  }
  
  ######## end defaults ########
  
  
  
}