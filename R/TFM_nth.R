#' Apply an nth order transformation
#'
#' Apply a mathematical transformation, e.g. new = a + b(old) + c(old)^2 + c(old)^3 + ...etc
#'
#' @export
#' @param values a vector of numerical values
#' @param coeffs a vector of coefficients, from 0 to nth order (e.g., c(2,3,4,5) == 5x^3 + 4x^2 + 3x + 2)
#' @keywords transformation
#' @examples new <- TFM_nth(values, coeffs = c(1, 0.1, 0.01, 0.001) )
#'
#'

TFM_nth <- function(values, coeffs){


  ######## set defaults ########

  if (missing(coeffs)){
    coeffs <- NA
  }

  ######## end set defaults ########

    newVals <- 0


      for (z in 1:length(coeffs)){

    newVals <- newVals + coeffs[z] * values^(z-1)

           } # end coeffs loop


  return(newVals)


} ######## end function ########
