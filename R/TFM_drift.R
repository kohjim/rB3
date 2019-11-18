#' Apply drift correction
#'
#' Change offset and slope over time to account for linear measurement drift
#'
#' @export
#' @param values a vector of numeric values
#' @param DateTimes a vector of POSIXct timestamps corresponding to values vector
#' @param valsRef a vector of reference calibration points (low, high)
#' @param valsStart a vector of low and high values at start of period, to be adjusted to meet 'valsRef'. Defaults to = valsRef.
#' @param valsEnd a vector of low and high values at end of period, to determine linear drift adjustment to meet 'valsRef'
#' @keywords transformations
#' @examples new <- TFM_drift(values,DateTimes, valsRef = c(0,100), valsStart = c(-10,90), valsEnd = c(7,85) )
#'

TFM_drift <- function(values,DateTimes, valsRef, valsStart, valsEnd){


  ######## set defaults ########
  if (missing(valsStart)){
    valsStart <- valsRef
  }

  ######## end set defaults ########

  ######### FUNCTION GOES HERE ###########

  # find slope and offset adjustment at start of period
  offsetStart <- valsRef[1] - valsStart[1]
  slopeStart  <- (valsRef[2] - valsRef[1] ) / (valsStart[2] - valsStart[1])

  # correct the time-series for the initial differences
  values <- values * slopeStart + offsetStart

  # find slope and offset adjustment at end of period
  offsetSpan <- valsStart[1] - valsEnd[1]
  slopeSpan  <- 1 - (valsStart[2] - valsStart[1]) / (valsEnd[2] - valsEnd[1])

  # date range in seconds
  dateStart <- min(as.numeric(DateTimes) )
  dateEnd <- max(as.numeric(DateTimes) )
  dateSpan <- dateEnd - dateStart

  timeElapsed <- (as.numeric(DateTimes) - dateStart) / dateSpan

  # find slope and offset for each row
  offsets <- timeElapsed * offsetSpan
  slopes <- 1 - (timeElapsed * slopeSpan)

  # apply adjustments through time
  values <- values * slopes + offsets

  # return the modified time-series
  return(values)

  ######## end function ########
}
