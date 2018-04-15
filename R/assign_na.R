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
  #@  Chris McBride: chris@limnotrack.com , 2018-03-13  @#
  #@  Kohji Muraoka: kohji.muraoka@gmail.com @#

  ######## log making 1 ######## 
  # check if DF is a list 
  # default: input log does not exist
  
  log_exist <- FALSE
  inLog <- NULL
  
  if (!is.data.frame(DF_in)){
    inLog <- DF_in[[2]]
    outLog <- inLog
    log_exist <- TRUE
    
    DF_in <- DF_in[[1]]
  }
  ######## end log making 1 ######## 
  

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
  ######## end defaults ########
  
  
  ######## function ########
  # identify the elements in the array

  outs.idElToModify <- idElToModify(DF_in, startDate, endDate, varNames)
  
  # decompose the list
  rowLocs <- outs.idElToModify[[1]]
  colLocs <- outs.idElToModify[[2]]
  
  # add NA to the elements
  DF_in[rowLocs,colLocs] <- NA
  ######## end function ########
  
  
  ######## log making 2 ######## 
  if (!is.na(logID)){
    
    thisLog <- DF_in
    thisLog[,-1] <- NA
    thisLog[rowLocs,colLocs] <- logID
      
    outLog <- mkLongLog(inLog,thisLog,logID)
    
  }
  ######## end log making 2 ######## 
  
  
  ######## return with or without Log ########
  if (!is.na(logID) | log_exist){
    
    return(list(DF_in,outLog))
    
  } else {
    
    return(DF_in)
  }
  ######## end return with or without Log ########
  
  # gsub("NA,", "", outLog, fixed = TRUE)
  # gsub(",NA", "", outLog, fixed = TRUE)
  
  # make log file
  # a <- data.frame(NA,2,3,4)
  # b <- data.frame(2,3,4,5)
  # cc <- paste(a,b, sep=",")
  # gsub("NA,", "", c, fixed = TRUE)
  
} # end function