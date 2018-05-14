#' Remove repeated values
#'
#' This function assign NA to the repeated data more than maxRep times continuous
#' 
#' @export
#' @param DF_in data frame input
#' @param metaD metadata list
#' @param startDate start date
#' @param endDate endDate
#' @param varNames list of variable names or keywords
#' @param minVal minimum value permitted
#' @param cndFile conditoin file path
#' @param logID assign log ID
#' @param plotPath plot figure of before and after
#' @keywords wrangling
#' @examples newDF <- exclude_vars(myDF,metaData,varNames = c("pH","wndDir"))
#' 
#' 

filter_stall <-  function(DF_in, metaD, maxRep, varNames, startDate, endDate, cndFile, logID, plotPath) {

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
  
  if (missing(maxRep)){
    maxRep <- NULL
  }
  
  if (missing(plotPath)){
    plotPath <- NULL
  }
  
  ######## end defaults ########

  ######## function ########
  # list of variable names
  varList <- c("maxRep") # number of points being stuck in the same number
  
  # col names for later use
  DFColNames <- colnames(DF_in[,])

  if (!is.null(cndFile)){
    isUseCndFile <- TRUE
    condFilePath <- cndFile
    cnd <- readCndFile(condFilePath)
    
  } else {
    isUseCndFile <- FALSE
    maxRep <- maxRep
  }

    # identify the elements in the array
  outs.idElToModify <- idElToModify(DF_in, startDate = startDate, endDate = endDate, varNames = varNames)
  
  # decompose the list
  rowLocs_bak <- outs.idElToModify[[1]]
  rowLocsNums <- which(rowLocs_bak)
  
  colLocs <- outs.idElToModify[[2]]
  colLocsNums <- which(colLocs)
  
  # prepare DF to record all row locs being modified
  rowLocs <- DF_in[colLocs]
  rowLocs[] <- FALSE
  
  for (i in 1:length(colLocsNums)){
    # name of the column to be changed
    thisColName <- DFColNames[colLocsNums[i]]
    
    if (isUseCndFile){
      
      # identify condition file condition information locations
      cndColLocNum <- which(grepl(thisColName,colnames(cnd),ignore.case = TRUE))
      cndRowLocNum <- which(grepl(varList[1],rownames(cnd),ignore.case = TRUE))
      
      # identify conditions
      maxRep <- as.numeric(as.character(cnd[cndRowLocNum,cndColLocNum]))

    } else {

    }

    ##### algorithm to identify repeats and remove ##### 

    # DF_in column to modify
    thisDf <- data.frame(DF_in[,colLocsNums[i]])
    
    # compare with the next element and if true assign same ID
    neighbourComp <- c(TRUE,thisDf[-1L,1] != thisDf[-length(thisDf[,1]),1])
    neighbourComp[is.na(thisDf[,1])] <- TRUE
    neighbourComp[is.na(neighbourComp)] <- TRUE
    
    neighbourComp <- with(thisDf, cumsum(neighbourComp))

    repeatCounts <- ave(seq_along(thisDf[,1]), 
                        neighbourComp, FUN=seq_along)
    
    # find repeats that exceeded use set max repeats
    excessRepeatLocs <- which(repeatCounts >= maxRep & !is.na(thisDf[,1]))
    
    # identify all sequence locations that contains too many repeats

    thisDf$rowsToChange <- FALSE
    thisDf$rowsToChange[which(neighbourComp %in% neighbourComp[excessRepeatLocs])] <- TRUE
    
    # for (j in 1:length(excessRepeatLocs)){
    #   thisDf$rowsToChange[
    #     neighbourComp == neighbourComp[excessRepeatLocs[j]]] <- TRUE
    # }

    # outside user date range = do not modify
    thisDf$rowsToChange[-rowLocsNums] <- FALSE

    # replace by NA
    DF_in[thisDf$rowsToChange,colLocsNums[i]] <- NA
    
    rowLocs[,i] <- thisDf$rowsToChange[]
    
    ######## log making 2A ######## 
    if (!is.na(logID)){
      thisLog[thisDf$rowsToChange,colLocsNums[i]] <- logID
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
    plotDiff(DF_bak, DF_in, colNum = colLocsNums, plotPath = plotPath, custom_dpi = 150, taskName = "filter_stall")
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