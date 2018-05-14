#' Custom fun: template 1
#'
#' This custom function plots variables on one ggplot
#' 
#' @export
#' @param DF_in data frame input
#' @param startDate start date
#' @param endDate endDate
#' @param varNames variable names or keywords
#' @param logID assign log ID
#' @param plotPath plot figure of before and after
#' @keywords customFun
#' @examples newDF <- custom_fun_template_1(DF_in,metaD,startDate,endDate,varNames,logID,plotPath)
#' 
ggplot_allVars <- function(DF_in,metaD,varNames,startDate,endDate){
  
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
  
  # DF_bak <- DF_in # to be used in plotDiff
  # 
  # if (missing(logID)){
  #   logID <- NA
  # } else {
  #   thisLog <- DF_in
  #   thisLog[,-1] <- NA
  # }
  ######## end log making 1 ######## 
  
  ######## set defaults ######## 
  if (missing(startDate)){
    startDate <- DF_in$DateTime[1]
  }
  
  if (missing(endDate)){
    endDate <- DF_in$DateTime[length(DF_in$DateTime)]
  }

  if (missing(varNames)){
    varNames$Vars <- "All"
  }
  
  #   if (missing(varNames)){
  #   varArgs$Vars <- "All"
  # }
  # 

  
  ######## end set defaults ######## 
  
  
  ######## find elements to modify ######## 
  
  outs.idElToModify <- idElToModify(DF_in,
                                    startDate = startDate,
                                    endDate = endDate,
                                    varNames = varNames)
  # decompose the list
  rowLocs <- outs.idElToModify[[1]]
  rowLocsNums <- which(rowLocs)
  colLocs <- outs.idElToModify[[2]]
  colLocsNums <- which(colLocs)
  
  ######## find elements to modify ######## 
  
  
  
  ########          ########
  ######## function ######## 

  # write function here
  
  #######MAKE SOME GGPLOTS################
  # browser()
  plotDF <- DF_in[rowLocsNums,c(1,colLocsNums)]
  
  plotAll <- tidyr::gather(plotDF,var, value, 2:ncol(plotDF))
  
  ggplot2::ggplot(plotAll, ggplot2::aes(x = DateTime, y = value)) +
    ggplot2::geom_line(ggplot2::aes(color = var)) +
    ggplot2::ylab("Value") +
    ggplot2::xlab("Date")
  
#  ggplot2::ggsave(paste0(plotPath, metaD$lakeName,"_All.png"), height = 30, width = 10, dpi = 400)
  # ggplot2::ggsave("figures/Tutira_TmpWtr.png", height = 30, width = 15, dpi = 400)  
  
 
  ######## end function ######## 
  ########              ########
  
  
  
  # ######## save plot diff ######## 
  # if (!is.null(plotPath)){  
  #   plotDiff(DF_bak, DF_in,
  #            colNum = colLocsNums,
  #            plotPath = plotPath,
  #            custom_dpi = 150,
  #            taskName = "taskName")   ######### Change name here (figure's title will contain this info)
  # }
  # ######## save plot diff ######## 
  # 
  # 
  # ######## log making 2 ######## 
  # if (!is.na(logID)){
  #   thisLog[rowLocs,colLocs] <- logID
  #   outLog <- mkLongLog(inLog,thisLog,logID)
  # }
  # ######## end log making 2 ######## 
  # 
  # 
  # ######## return with or without Log ########
  # if (!is.na(logID) | log_exist){
  #   
  #   return(list(DF_in,outLog))
  #   
  # } else {
  #   
  #   return(DF_in)
  # }
  # ######## end return with or without Log ########
}