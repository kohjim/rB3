#' Fix linear drift (for sensor maintenance calibration)
#'
#' Linear drift correction. First offset and span will be assumed to be the right values.
#' If span is missing, only offset will be used.
#' 
#' new_value(t) = (value(t) - offset(t)) / span(t) * initial_span + initial_offset
#' 
#' @export
#' @param DF_in data frame input
#' @param startDate start date
#' @param endDate endDate
#' @param varNames variable names or keywords
#' @param colNum variable column numbers
#' @param offsets pair of offset values corresponding to startDate and endDate (i.e. lower limits)
#' @param spans pair of spans values corresponding to startDate and endDate (i.e. value ranges from offset; upper limits - lower limits)
#' @param plotPath plot paths with name (e.g. figures/Rotorua_)
#' @keywords customFun
#' @examples newDF <- fix_drift(DF_in = myDF, startDate = "2007/1/1", endDate = "2007/7/1", colNum = 15, offsets = c(1,3), span = c(5,6), logID = 5, plotPath = "figures/Rotorua_")
#' 
fix_drift <- function(DF_in, metaD, startDate, endDate, varNames, colNum, offsets, spans, logID, plotPath){
  
  ######## log making 1 ######## 
  # check if DF is a list (i.e. with log)
  # default: input log does not exist
  
  log_exist <- FALSE
  inLog <- NULL
  
  if (!is.data.frame(DF_in)){
    inLog <- DF_in[[2]]
    outLog <- inLog
    log_exist <- TRUE
    
    DF_in <- DF_in[[1]]
  }
  
  DF_bak <- DF_in # to be used in plotDiff
  
  if (missing(logID)){
    logID <- NA
  } else {
    thisLog <- DF_in
    thisLog[,-1] <- NA
  }
  ######## end log making 1 ######## 
  
  ######## set defaults ######## 
  if (missing(startDate)){
    startDate <- DF_in$DateTime[1]
  }
  
  if (missing(endDate)){
    endDate <- DF_in$DateTime[length(DF_in$DateTime)]
  }
  
  if (missing(varNames)){
    varNames <- "All"
  }
  
  if (missing(plotPath)){
    plotPath <- NULL
  }
  
  if (missing(spans)){
    spans <- NULL
  }
  
  ######## end set defaults ######## 
  
  
  
  ######## find elements to modify ######## 
  
  outs.idElToModify <- idElToModify(DF_in,
                                    startDate = startDate,
                                    endDate = endDate,
                                    varNames = varNames)
  # decompose the list
  rowLocs <- outs.idElToModify[[1]]
  rowLocsNums <- which(rowLocs)
  
  # priority = colNum 
  if (missing(colNum)){
    colLocs <- outs.idElToModify[[2]]
    colLocsNums <- which(colLocs)
    
  } else {
    
    colLocsNums <- colNum
  }
  
  ######## find elements to modify ######## 
  
  
  
  ########          ########
  ######## function ######## 

  for (i in 1:length(colLocsNums)){
    
    this_vector <- DF_in[rowLocsNums,colLocsNums[i]]
    this_ts <- as.data.frame(1:length(this_vector))
    colnames(this_ts) <- "x"
      
    # poly fit model for lower limits
    x <- c(1,length(this_vector))
    y <- offsets
    
    lowerLim <- lm(y ~ poly(x, degree = 1))
    lowerLim_vector <- predict(lowerLim, this_ts)
    
    if (!is.null(spans)){
      
      # drift correction with magnitude variance
      # poly fit model for upper limits

      y <- offsets + spans
      upperLim <- lm(y ~ poly(x, degree = 1))
      upperLim_vector <- predict.lm(upperLim,this_ts)
      
      spans_vector <- upperLim_vector - lowerLim_vector
      
      new_vector <- (this_vector - lowerLim_vector) / spans_vector * spans[1] + offsets[1]

    } else {
      
      # drift correction without magnitude variance
      new_vector <- (this_vector - lowerLim_vector) + offsets[1]
    }
    
    # assign new values
    DF_in[rowLocsNums,colLocsNums[i]] <- new_vector
  }
  
 
  ######## end function ######## 
  ########              ########
  
  ######## save plot diff ######## 
  if (!is.null(plotPath)){  
    plotDiff(DF_bak, DF_in,
             colNum = colLocsNums,
             plotPath = plotPath,
             custom_dpi = 150,
             taskName = "fix_drift")   ######### Change name here (figure's title will contain this info)
  }
  ######## save plot diff ######## 
  
  
  ######## log making 2 ######## 
  if (!is.na(logID)){
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
}