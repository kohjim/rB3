#' Remove values outside of specified minimum and maximum bounds
#'
#' Assigns NA to values outside the boundaries
#'
#' @param values a vector of numeric values
#' @param minVal minimum allowed value
#' @param maxVal maximum allowed value
#' @keywords outlier detection
#' @export
#' @examples new <- FLT_bounds(old, minVal = 10, maxVal = 28)

FLT_bounds <- function(values, minVal, maxVal) {

  ######## defaults ########
  if (missing(minVal)){
    minVal <- -1000000000000000000
  }

  if (missing(maxVal)){
    maxVal <- 1000000000000000000
  }

  ######## end defaults ########


  ######## function ########

  ## detect and replace outside bounds
  values[values < minVal | values > maxVal] <- NA

  # return the new value set
  return(values)

  ######## end function ########
}
