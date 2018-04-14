#' Assign NA to the specified dates range and variables
#'
#' This function assign NA to the specific dates range and variables
#' 
#' @import tidyr


mkLongLog <- function(inLog,thisLog){
  
  browser()
  library(tidyr)
  
  dfl <- gather(thisLog, key = var, 1,-DataTime)
  
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