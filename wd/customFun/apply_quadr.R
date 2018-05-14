#' Custom fun: capply_quadr
#'
#' This custom function applies a quadratic transformation to a specifed data range
#' of the format newVal = oldVal*coeffA^2 + oldVal*coeffB + coeffC
#' 
#' @export
#' @param DF_in data frame input
#' @param startDate start date
#' @param endDate endDate
#' @param logID assign log ID
#' @param plotPath plot figure of before and after
#' @keywords customFun
#' @examples newDF <- DOsat2DOmg_ZebraTechDOpto(DF_in = myDF,DOmgColName = "DOconc_d00050", DOsatColName = "DOpsat_d00050", TColName = "TmpDOs_d00050")
#' 
#' 

apply_quadr <- function(DF_in,startDate,endDate,varNames, coeffs, logID, plotPath){
  
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
  
  DF_bak <- DF_in
  
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
  
  if (missing(plotPath)){
    plotPath <- NULL
  }
  ######## end set defaults ######## 
  
  
  ######## identify locs ######## 
  # idElToModify does not care about the order so require running three times individually
 
  outs.idElToModify <- idElToModify(DF_in, startDate, endDate, varNames)
  
  ######## end identify locs ######## 
  
  # decompose the list
  rowLocs <- outs.idElToModify[[1]]
  rowLocsNums <- which(rowLocs)
  colLocs <- outs.idElToModify[[2]]
  colLocsNums <- which(colLocs)
#  browser()
  oldVal <- DF_in[rowLocsNums,colLocsNums]
  
  newVal <- oldVal^2*coeffs[3] + oldVal*coeffs[2] + coeffs[1]
    
    # replace by NA
    DF_in[rowLocsNums,colLocsNums] <- newVal
    # }
  ######## save plot diff ######## 
  if (!is.null(plotPath)){  
    plotDiff(DF_bak, DF_in,
             colNum = DOmgColLoc,
             plotPath = plotPath,
             custom_dpi = 150,
             taskName = "DOsat2DOmg")
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