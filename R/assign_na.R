#' Assign NA to the specified dates range and variables
#'
#' This function assign NA to the specific dates range and variables
#' 
#' @param DF_in data frame input
#' @param metaD metadata list
#' @param startDate start date
#' @param endDate endDate
#' @param varNames list of variable names or keywords
#' @keywords wrangling
#' @export
#' @examples newDF <- exclude_vars(myDF,metaData,varNames = c("pH","wndDir"))

assign_na <- function(DF_in, metaD, startDate, endDate, varNames, logID) {
  # This function removes data in specified dates. Require standard log file inputs.
  #
  # Example of log file
  #
  # [Label to apply], [function], [start date], [end date]
  # All, Delete, 2007-08-29- 10:45:00, 2007-09-07- 11:15:00
  # TmpWtr, Delete,2008-08-02- 19:00:00, 2008-08-02- 19:00:00 
  #
  
  #@  Chris McBride: chris@limnotrack.com , 2018-03-13  @#
  #@  Kohji Muraoka: kohji.muraoka@gmail.com @#

  ######## check if DF is a list ######## 
  # default: input log does not exist
  log_exist <- FALSE
  inLog <- NULL
  
  if (!is.data.frame(DF_in)){
    inLog <- DF_in[[2]]
    log_exist <- TRUE
    
    DF_in <- DF_in[[1]]
  }

  ######## defaults ########
  if (missing(startDate)){
    varArgs$StartDate <- DF_in$DateTime[1]
  }
  
  if (missing(endDate)){
    varArgs$EndDate <- DF_in$DateTime[length(DF_in$DateTime)]
  }
  
  if (missing(varNames)){
    varArgs$Vars <- "All"
  }
  
  if (missing(logID)){
    logID <- NA
  }
  
  ######## function ########
  # identify the elements in the array

  outs.idElToModify <- idElToModify(DF_in, startDate, endDate, varNames)
  
  # decompose the list
  rowLocs <- outs.idElToModify[[1]]
  colLocs <- outs.idElToModify[[2]]
  
  # add NA to the elements
  DF_in[rowLocs,colLocs] <- NA

  
  if (!is.na(logID)){
    
    thisLog <- DF_in
    thisLog[,-1] <- NA
    thisLog[rowLocs,colLocs] <- logID
      
    outLog <- mkLongLog(inLog,thisLog)
    
    # if (log_exist){
    #   
    #   browser()
    #   
    #   outLog <- inLog
    #   outLog_NoDates <- outLog[,-1]
    #   thisLog_NoDates <- thisLog[,-1]
    #   
    #   outLog_NoDates[!is.na(thisLog_NoDates)] <- 
    #     paste(outLog_NoDates[!is.na(thisLog_NoDates)],
    #           thisLog_NoDates[!is.na(thisLog_NoDates)],
    #           sep=",")
    #   
    # } else {
    #   
    #   outLog <- thisLog
    # }
  }
  
  if (!is.na(logID)){
    
    return(list(DF_in,outLog))
    
  } else if (log_exist & is.na(logID)) {
    
    return(list(DF_in,inLog))
    
  } else {
    
    return(DF_in)
  }
  
  
  # gsub("NA,", "", outLog, fixed = TRUE)
  # gsub(",NA", "", outLog, fixed = TRUE)
  
  # make log file
  # a <- data.frame(NA,2,3,4)
  # b <- data.frame(2,3,4,5)
  # cc <- paste(a,b, sep=",")
  # gsub("NA,", "", c, fixed = TRUE)
  
} # end function