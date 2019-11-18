#' Assign a value or NA to the specified dates range and variables
#'
#' Specify subset of dates, values and variables to modify
#'
#' @param values a vector of numeric values
#' @param newVal value to replace this data with (NA or numeric)
#' @keywords data editing
#' @export
#' @examples new <- TFM_replace(values,NA)

TFM_replace <- function(values, newVal) {

  ######## defaults #########
  if (missing(newVal)){
    newVal <- NA
  }

  ######## end defaults ########


  ######## function ########

  values <- newVal


  # return the new Values
  return(values)

  ######## end function ########
}
