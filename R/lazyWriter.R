#' Simple table writing function
#'
#' Write table csv file. Also write logName file in Rdata format.
#' 
#' @param DF_in data frame input
#' @param metaD metadata list
#' @param startDate start date
#' @param endDate endDate
#' @param varNames list of variable names or keywords
#' @param fileName filename (path) that csv file will be saved as
#' @param logName filename (path) that Rdata log file will be saved as
#' @keywords wrangling
#' @export
#' @examples newDF <- lazyWriter(DF_in,fileName)

lazyWriter <- function(DF_in, fileName, logName, startDate, endDate, varNames){
  
  # check if DF is a list 
  # default: input log does not exist
  
  log_exist <- FALSE
  inLog <- NULL
  
  if (!is.data.frame(DF_in)){
    inLog <- DF_in[[2]]
    DF_in <- DF_in[[1]]
  } 
 
  
  ######## defaults ########
  if (missing(startDate)){
    startDate <- DF_in$DateTime[1]
  }
  
  if (missing(endDate)){
    endDate <- DF_in$DateTime[length(DF_in$DateTime)]
  }
  
  writeLog <- TRUE
  if (missing(logName)){
    writeLog <- FALSE
  }
  
  if (missing(varNames)){
    varNames <- "All"
  }
  
  outs.idElToModify <- idElToModify(DF_in = DF_in, startDate = startDate, endDate = endDate, varNames = varNames)
  
  # decompose the list
  rowLocs <- outs.idElToModify[[1]]
  rowLocsNums <- which(rowLocs)
  colLocs <- outs.idElToModify[[2]]
  colLocsNums <- which(colLocs)
  
  DF_out <- as.data.frame(DF_in[rowLocsNums,c(1,colLocsNums)])
  
  ######## end defaults ########
  # browser()
  write.table(DF_out,fileName,
              sep = ",",
              append = FALSE,
              row.names = FALSE,
              quote = FALSE)
  
  if (writeLog){
    save(inLog, file=logName)
  }
}