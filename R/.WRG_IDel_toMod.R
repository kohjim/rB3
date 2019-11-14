#' Identify elements locations for the specified variable and dates ranges
#'
#' This sub-function identifies elements of the data frame that satisfies dates range and/or variable names,
#'
#' @export
#' @param dt.in data table input
#' @param dateRange vector giving start and end of date range to apply function
#' @param varNames variable names that will be selected
#' @keywords wrangling
#' @examples idElToModify(rB3in, dateStart = '2015-07-01', dateEnd = '2016-06-30', varNames = 'All')
#'

IDel_toMod <- function(dt.in, dateRange, varNames){

  ### DEFAULTS ####

  if (missing(dateRange)){
    dateRange <- c(NA,NA)
  }

  if (missing(varNames)){
    varNames <- 'All'
  }

  ### END DEFAULTS ####


  ### MAIN FUNCTION ####

  # coerce to data table, just in case
  dt.in <- data.table::data.table(dt.in)
  dt.in$DateTime <- as.POSIXct(dt.in$DateTime,
                                origin = "1970-01-01 00:00:00",
                                tz = "UTC"
                              )

  # set start and end dates
  if (is.na(dateRange[1])) {

    dateStart <- min(dt.in$DateTime)

  } else {

    dateStart <- as.POSIXct(
      dateRange[1],
      origin = "1970-01-01 00:00:00",
      tz = "UTC"
    )
  }

  if (is.na(dateRange[2])) {

    dateEnd <- max(dt.in$DateTime)

  } else {

  dateEnd <- as.POSIXct(
    dateRange[2],
    origin = "1970-01-01 00:00:00",
    tz = "UTC"
    )

  }

  ##### find the chunk of data to be selected

  modRows <- (dt.in$DateTime >= dateStart & dt.in$DateTime <= dateEnd) #|
     #     is.na(dt.in$DateTime)

  modRowNums <- which(modRows)

  modDates <- dt.in$DateTime[modRowNums]

        # identify columns affected by the task
        if (varNames[1] == "All") {

        modCols <- !grepl("DateTime",unlist(labels(dt.in)[2]))

        } else {

        # find vars applied and separate them (delimiter <- space)
        colLabs <- unlist(as.character(varNames))

        modCols <- grepl(paste(colLabs, collapse = "|"),unlist(labels(dt.in)[2]),ignore.case = TRUE)

        }

  modColNums <- which(modCols)

  modNames   <- names(dt.in[modColNums])

  # outList <- list(modDates,modRows,modRowNums,modCols,modColNums,modNames)
     # names(outList) <- c("DateTimes","rows","row_numbers","columns","column_numbers","column_names")
  outList <- list(modRowNums,modColNums)
     names(outList) <- c("row_numbers","column_numbers")

     return(outList)

} ### end function
