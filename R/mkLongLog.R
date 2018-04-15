#' Assign NA to the specified dates range and variables
#'
#' This function assign NA to the specific dates range and variables
#' 
#' @import tidyr
#' @export

mkLongLog <- function(inLog, thisLog, logID){
  
  thisLog_long <- tidyr::gather(thisLog, key = varNames, value = thisLog, -DateTime)
  
  if (is.null(inLog)){
    outLog <- thisLog_long
    colnames(outLog)[3] <- logID
    
  } else {
    outLog <- cbind(inLog,thisLog_long$thisLog)
    colnames(outLog)[ncol(outLog)] <- logID
  }
  
  return(outLog)
  
  # inside log write file use these
  # gsub("NA,", "", outLog, fixed = TRUE)
  # gsub(",NA", "", outLog, fixed = TRUE)# # not run
  
}