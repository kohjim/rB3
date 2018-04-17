#' Custom fun: convert DOsat -> DOmg
#'
#' This custom function converts DOsat and temperature to DOmg
#' 
#' @export
#' @param DF_in data frame input
#' @param startDate start date
#' @param endDate endDate
#' @param DOmgColName column name of DOmg
#' @param DOsatColName column name of DOsat
#' @param TColName column name of TCOl
#' @param logID assign log ID
#' @param plotPath plot figure of before and after
#' @keywords wrangling
#' @examples newDF <- DOsat2DOmg_ZebraTechDOpto(DF_in = myDF,DOmgColName = "DOconc_d00050", DOsatColName = "DOpsat_d00050", TColName = "TmpDOs_d00050")
#' 
#' 

DOsat2DOmg_ZebraTechDOpto <- function(DF_in,startDate,endDate,DOmgColName, DOsatColName, TColName, plotPath){
  
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
    startDate <- DF_in[1,1]
  }
  if (missing(endDate)){
    endDate <- DF_in[length(DF_in[,1]),1]
  }
  if (missing(plotPath)){
    plotPath <- NULL
  }
  
  DF_bak <- DF_in
  ######## end set defaults ######## 
  
  
  ######## identify locs ######## 
  # idElToModify does not care about the order so require running three times individually
  # DO mg
  outs.idElToModify <- idElToModify(
    DF_in, 
    startDate = startDate, 
    endDate = endDate, 
    varNames = c(DOmgColName)
    )
  colLocs <- outs.idElToModify[[2]]
  DOmgColLoc <- which(colLocs)
  
  # DO sat
  outs.idElToModify <- idElToModify(
    DF_in, 
    startDate = startDate, 
    endDate = endDate, 
    varNames = c(DOsatColName)
  )
  colLocs <- outs.idElToModify[[2]]
  DOsatColLoc <- which(colLocs)
  DOsat <- DF_in[,DOsatColLoc]
  
  # temperature
  outs.idElToModify <- idElToModify(
    DF_in, 
    startDate = startDate, 
    endDate = endDate, 
    varNames = c(TColName)
  )
  colLocs <- outs.idElToModify[[2]]
  TColLoc <- which(colLocs)
  t <- DF_in[,TColLoc]
  
  ######## end identify locs ######## 
  
  
  ######## conversion ######## 
  DF_in[,DOmgColLoc] <- 
    (exp(-139.34411+
           ((157570.1*(1/( t +273.15)))+
              (-66423080*((1/( t +273.15))^2))+
              (12438000000*((1/( t +273.15))^3))+
              (-862194900000*((1/( t +273.15))^4)))
        )
    ) * DOsat *0.01
  ######## end conversion ######## 
  
  
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