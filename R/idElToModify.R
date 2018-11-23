#' Identify elements locations for the specified variable and dates ranges
#'
#' This sub-function identifies elements of the data frame that satisfies dates range and/or variable names,
#'
#' @export
#' @param rB3in data frame input
#' @param startDate start date
#' @param endDate end date
#' @param varNames variable names that will be selected
#' @keywords wrangling
#' @examples idElToModify(rB3in, startDate = '2015-07-01', endDate = '2016-06-30, varNames = 'All')
#'

idElToModify <- function(rB3in, startDate, endDate, varNames){

  startDate <- as.POSIXct(
    startDate,
    origin = "1970-01-01 00:00:00",
    tz = "UTC"
    )

  endDate <- as.POSIXct(
    endDate,
    origin = "1970-01-01 00:00:00",
    tz = "UTC"
    )

  # find the chunnk of data to be selected

  if (!is.na(startDate)) {

    # identify rows affected by the change
    df <- data.frame(rB3in[["qcDF"]])

    modRows <- (df$DateTime >= startDate &
      df$DateTime <= endDate) |
      is.na(df$DateTime)

    # identify columns affected by the task
    if (varNames[1] == "All"){
      modCols <- !grepl("DateTime",unlist(labels(df)[2]))

    } else {

      # find vars applied and separate them (delimiter <- space)
      colLabs <- unlist(as.character(varNames))
      modCols <- grepl(paste(colLabs, collapse = "|"),unlist(labels(df)[2]),ignore.case = TRUE)
    }

  } else {

    # browser()
    # when inputs are characters
    modRows <- 0

    colLabs <- unlist(as.character(varNames))
    modCols <- grepl(paste(colLabs, collapse = "|"),unlist(labels(rB3in)[2]),ignore.case = TRUE)
  }
  return(list(modRows,modCols))

} # end function
