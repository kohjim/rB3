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

lazyWriter <- function(DF_in,fileName,logName){
  
  ######## log making 1 ######## 
  # check if DF is a list 
  # default: input log does not exist
  
  log_exist <- FALSE
  inLog <- NULL
  
  if (!is.data.frame(DF_in)){
    inLog <- DF_in[[2]]
    DF_in <- DF_in[[1]]
  } 
  ######## end log making 1 ######## 
  
  ######## defaults ########
  writeLog <- TRUE
  if (missing(logName)){
    writeLog <- FALSE
  }
  ######## end defaults ########
  
  write.table(DF_in,fileName, sep = ",", eol = "\r\n", append = FALSE)
  
  if (writeLog){
    save(inLog,file=logName, append = FALSE)
  }
}