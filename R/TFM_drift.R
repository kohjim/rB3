#' Apply drift correction
#'
#' Change offset and slope over time to account for linear measurement drift
#'
#' @export
#' @param values a vector of numeric values
#' @param DateTimes a vector of POSIXct timestamps corresponding to values vector
#' @param valsRef a vector of reference calibration points (low, high)
#' @param valsStart a vector of low and high values at start of period, to be adjusted to meet 'valsRef'
#' @param valsEnd a vector of low and high values at end of period, to determine linear drift adjustment to meet 'valsRef'
#' @keywords transformations
#' @examples new <- TFM_drift(values,DateTimes, valsRef = c(0,100), valsStart = c(-10,90), valsEnd = c(7,85) )
#'

TFM_drift <- function(values,DateTimes, valsRef, valsStart, valsEnd){


  ######## set defaults ########
  # if (missing(startDate)){
  #   startDate <- rB3in[["qcDF"]]$DateTime[1]
  # }

  ######## end set defaults ########

  ######### FUNCTION GOES HERE ###########

  # find slope and offset adjustment at start of period
  offsetStart <- valsRef[1] - valsStart[1]
  slopeStart  <- (valsRef[2] - valsRef[1] ) / (valsStart[2] - valsStart[1])

  # find slope and offset adjustment at end of period
  offsetEnd <- valsRef[1] - valsEnd[1]
  slopeEnd  <- (valsRef[2] - valsRef[1]) / (valsEnd[2] - valsEnd[1])

  # date range in seconds
  dateStart <- min(as.numeric(DateTimes) )
  dateEnd <- max(as.numeric(DateTimes) )
  dateSpan <- dateEnd - dateStart

  timeElapsed <- (as.numeric(DateTimes) - dateStart) / dateSpan

  # find slope and offset for each row
  offsets <- timeElapsed *
  slopes <- seq(from = slopeStart, to = slopeEnd, length.out = length(rowLocsNums))



  # apply adjustments through time
  for (l in rowLocsNums) {

    df[l,colLocsNums[i]] <- df[l,colLocsNums[i]] * slopes[l] + offsets[l]
    }

    ## write drift-corrected data to highlighting DF
    hlDF[rowLocsNums,colLocsNums[i]] <- df[rowLocsNums,colLocsNums[i]]

    ### write to same portion of logDF
    rB3new[["logDF"]] [rowLocsNums,colLocsNums[i]] <- ifelse(is.na(rB3new[["logDF"]] [rowLocsNums,colLocsNums[i]]),
                                                             logID,
                                                             paste0(rB3new[["logDF"]] [rowLocsNums,colLocsNums[i]], ' : ',logID ))



  # return the modified rB3 object
  return(rB3new)

  ######## end function ########
}
