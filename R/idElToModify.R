#' Identify elements locations for the specified variable and dates ranges
#'
#' This sub-function identifies elements of the data frame that satisfies dates range and/or variable names,
#' 
#' @export
#' @param DF_in data frame input
#' @param metaD metadata list
#' @param startDate start date
#' @param endDate end date
#' @keywords wrangling
#' @examples newDF <- exclude_vars(myDF,metaData,varNames = c("pH","wndDir"))
#' 

idElToModify <- function(DF_in, startDate, endDate, varNames){
  # retun location information TF information of the data from the task information
  # either taskDetails with full task information or character elements separated by spaces are required
  #
  # require startDate endDate taskDetails$Vars
  #
  # Usage
  # outs <- idElToModify(DF_in, taskDetails)
  #   rowLocs <- outs[[1]]
  #   colLocs <- outs[[2]]
  #
  # outs <- idElToModify(DF_in, "tmp DO")
  #   colLocs <- outs[[2]]
  #
  # if (missing(varNames)){
  #   varNames = "All"
  # }
  
  startDate <- as.POSIXct(
    startDate,
    origin = "1970-01-01 00:00:00",
    tz = "Etc/GMT+12"
    )  
  
  endDate <- as.POSIXct(
    endDate,
    origin = "1970-01-01 00:00:00",
    tz = "Etc/GMT+12"
  )

  if (!is.na(startDate)) {
    
    # identify rows affected by the change
    delRows <- (DF_in$DateTime >= startDate &
      DF_in$DateTime <= endDate) | 
      is.na(DF_in$DateTime)

    # identify columns affected by the task
    if (varNames[1] == "All"){
      delCols <- !grepl("DateTime",unlist(labels(DF_in)[2]))
      
    } else {
      
      # find vars applied and separate them (delimiter <- space)
      colLabs <- unlist(as.character(varNames))
      delCols <- grepl(paste(colLabs, collapse = "|"),unlist(labels(DF_in)[2]),ignore.case = TRUE)
    }
    
  } else {
    
    # browser()
    # when inputs are characters
    delRows <- 0
    
    colLabs <- unlist(as.character(varNames))
    delCols <- grepl(paste(colLabs, collapse = "|"),unlist(labels(DF_in)[2]),ignore.case = TRUE)
  }
  return(list(delRows,delCols))
  
} # end function