#' Find values' rate of change exceeded maxRoc value and assign NA
#'
#' This function assign NA to the specific dates range and variables

#' @export
#' @param DF_in data frame input
#' @param metaD metadata list
#' @param maxRoc rate of change per timestep
#' @param startDate start date
#' @param endDate end date
#' @param varNames list of variable names or keywords
#' @param cndFile condition file path
#' @keywords wrangling
#' @examples newDF <- maxRoc(myDF,metaData,varNames = c("pH","wndDir"))
#' 
#' 

filter_roc <-  function(DF_in,  metaD, maxRoc, varNames, startDate, endDate, cndFile, logID, plotPath) {
  
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
  
  DF_bak <- DF_in
  
  if (missing(logID)){
    logID <- NA
  } else {
    thisLog <- DF_in
    thisLog[,-1] <- NA
  }
  ######## end log making 1 ######## 
  
  
  ######## defaults ########

  if (missing(startDate)){
    startDate <- DF_in$DateTime[1]
  }
  
  if (missing(endDate)){
    endDate <- DF_in$DateTime[length(DF_in$DateTime)]
  }
  
  if (missing(varNames)){
    varNames <- "All"
  }
  
  if (missing(cndFile)){
    cndFile <- NULL
  }
  
  if (missing(maxRoc)){
    maxRoc <- NULL
  }
  
  ######## end defaults ########
  
  ######## function ########

  # list of variable names in the cond file 
  varList <- c("maxRoc")
  
  # col names for later use
  DFColNames <- colnames(DF_in[,])

  if (!is.null(cndFile)){
    
    isUseCndFile <- TRUE
    condFilePath <- cndFile
    cnd <- readCndFile(condFilePath)
    
  } else {
    
    isUseCndFile <- FALSE
    maxRoc <- maxRoc
    
  }
  
  # identify the elements in the array
  outs.idElToModify <- idElToModify(DF_in, startDate = startDate, endDate = endDate, varNames = varNames)
  
  # decompose the list
  rowLocs <- outs.idElToModify[[1]]
  rowLocsNums <- which(rowLocs)
  colLocs <- outs.idElToModify[[2]]
  colLocsNums <- which(colLocs)
  
  
  for (i in 1:length(colLocsNums)){
    # name of the column to be changed
    thisColName <- DFColNames[colLocsNums[i]]
    
    if (isUseCndFile){
      
      # identify condition file condition information locations
      cndColLocNum <- which(grepl(thisColName,colnames(cnd),ignore.case = TRUE))
      cndRowLocNum <- which(grepl(varList[1],rownames(cnd),ignore.case = TRUE))
      
      # identify conditions
      maxRoc <- cnd[cndRowLocNum,cndColLocNum]
      
      # find conditioned location in the DF_in
      
      thisVector <- DF_in[,colLocsNums[i]]
      rowsToChange <- as.logical(thisVector)
      
      d_thisVector <- thisVector[1:length(thisVector),] - thisVector[-1,]
      
      if (maxRoc >= 0){
        rowsToChange[-1,] <- d_thisVector > maxRoc
      } else {
        rowsToChange[-1,] <- d_thisVector < maxRoc
      }
      
      # replace by NA
      DF_in[rowsToChange,colLocsNums[i]] <- NA
      
    } else {
      # find conditioned location in the DF_in
      thisVector <- DF_in[,colLocsNums[i]]
      rowsToChange <- as.logical(thisVector)
      
      d_thisVector <- thisVector[1:length(thisVector),] - thisVector[-1,]
      
      if (maxRoc >= 0){
        rowsToChange[-1,] <- d_thisVector > maxRoc
      } else {
        rowsToChange[-1,] <- d_thisVector < maxRoc
      }
      
      rowsToChange <- intersect(rowLocsNums,rowsToChange)
      
      # replace by NA
      DF_in[rowsToChange,colLocsNums[i]] <- NA
    }
    
    ######## log making 2A ######## 
    
    if (!is.na(logID)){
      thisLog[rowsToChange,colLocsNums[i]] <- logID
    }
    
    ######## end log making 2A ######## 
  }

  
  ######## log making 2B ######## 
  if (!is.na(logID)){  
    outLog <- mkLongLog(inLog,thisLog,logID)
  }
  ######## end log making 2B ######## 
  
  ######## save plot diff ######## 
  if (!is.null(plotPath)){  
    plotDiff(DF_bak, DF_in, colNum = colLocsNums, plotPath = plotPath, custom_dpi = 150, taskName = "filter_roc")
  }
  ######## save plot diff ######## 

  ######## return with or without Log ########
  if (!is.na(logID) | log_exist){
    
    return(list(DF_in,outLog))
    
  } else {
    
    return(DF_in)
  }
  ######## end return with or without Log ########
  
} # end function