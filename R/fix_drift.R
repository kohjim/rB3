#' Fix linear drift (for sensor maintenance calibration)
#'
#' Linear drift correction. First offset and span will be assumed to be the right values.
#' If span is missing, only offset will be used.
#' 
#' new_value(t) = (value(t) - offset(t)) / span(t) * initial_span + initial_offset
#' 
#' @export
#' @param rB3in data frame input
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
fix_drift <- function(rB3in, startDate, endDate, varNames, colNum, offsets, spans, logID, Reason, showPlot, savePlot){
  
  
  ######## set defaults ######## 
  if (missing(startDate)){
    startDate <- rB3in[["qcDF"]]$DateTime[1]
  }
  
  if (missing(endDate)){
    endDate <- rB3in[["qcDF"]]$DateTime[length(rB3in[["qcDF"]]$DateTime)]
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
  
  outs.idElToModify <- idElToModify(
    rB3in,
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
    
    colLocs <- !as.logical(1:length(outs.idElToModify[[2]]))
    colLocs[colNum] <- TRUE
    colLocsNums <- colNum
  }
  
  # extract DF
  DF_in <- rB3in[["qcDF"]]
  
  # write to the logKey
  writeLog(rB3in, logID, funName = "fix_drift", Reason = "Drift values fixed" )
  
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
    
    rB3in[["logDF"]][rowLocsNums,colLocsNums[i]] <- logID
  }
  
 
  ######## end function ######## 
  ########              ########
  
  ##### plots #######
  
  rB3plot <- rB3in
  
  rB3plot[["preDF"]] <- rB3plot[["qcDF"]]
  rB3plot[["qcDF"]] <- DF_in
  
  
  # generate plot, if specified
  
  if (showPlot == TRUE | !is.null(savePlot)) {
    prePostPlot(rB3plot,
                startDate,
                endDate,
                varNames = varNames,
                srcColour = 'grey',
                preColour = 'red',
                qcColour = 'blue',
                showPlot = showPlot,
                savePlot = savePlot,
                dpi = 200)
  }
  
  ##### end plots #######
  
  # return rB3 obj
  rB3in[["qcDF"]] <- DF_in
  
  return(rB3in)
}