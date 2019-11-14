#' Remove consecutive repeated values
#'
#' Specify maximum number of allowable consecutive identical values, or read from the 'ctrls' (which have been read from source dataset headers)
#'
#' @param values a vector of numeric values
#' @param na.omit TRUE = ignore NA when searching for repeated values (i.e. NA counts as repeated value)
#' @param maxReps maximum allowed consecutive values
#' @keywords quality control filter
#' @export
#' @examples new <- FLT_reps(values, na.rm = T, maxReps = 20)

FLT_reps <- function(values, na.omit, maxReps) {

  ######## defaults ########
  if (missing(na.omit)){
    na.omit <- TRUE
  }

  ######## end defaults ########


  ######## function ########


  ## algorithm to identify repeats and remove ##

  # compare with the next element and if true assign same ID
  neighbourComp <- c(TRUE,values[-1L] != values[-length(values)])


  # if..
  if (na.omit == FALSE) {

    # then treat NA as a new unique value
    neighbourComp[is.na(values)] <- TRUE
    neighbourComp[is.na(neighbourComp)] <- TRUE

  }

  neighbourComp <- cumsum(neighbourComp)

  repeatCounts <- ave(seq_along(values),
                      neighbourComp, FUN=seq_along)

  # find repeats that exceeded max repeats
  excessRepeatLocs <- which(repeatCounts >= maxReps)

  # delete the repeats
  values[excessRepeatLocs] <- NA


  return(values)

}  ######## end function ########

