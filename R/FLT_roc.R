#' Remove values that exceed rate of change - replace with NA
#'
#' Specify a maximum roc
#'
#' @param values a vector of numeric values
#' @param DateTimes a vector of POSIXct timestamps corresponding to values vector
#' @param maxRoc maximum allowed rate of change (units per minute)
#' @keywords outlier detection
#' @export
#' @examples new <- FLT_roc(values, DateTimes, ROC = 1.5)

FLT_roc <- function(values, DateTimes, maxRoc) {

  ######## defaults ########
  if (missing(maxRoc)){
    maxRoc <- 10000000000
  }

  ######## end defaults ########


  ######## function ########

  valDiffs <- c( 0, diff(values) )
  timeDiffs <- c(0,diff(as.numeric(DateTimes)) ) / 60
  valRoc    <- valDiffs / timeDiffs                       # differences in units pre minute

  values[valRoc > maxRoc] <- NA                           # replace exceedances with NA

  return(values)

}
