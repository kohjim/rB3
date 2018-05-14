#' Custom fun: template 1
#'
#' This custom function produces a ggplot of all water temperature depths, faceted by hydrological year
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
ggplot_TmpWtr_hydroyr <- function(DF_in,metaD,startDate,endDate,plotPath){
  
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
  

    varNames <- "TmpWtr"

  
  if (missing(plotPath)){
    plotPath <- NULL
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
  colLocs <- outs.idElToModify[[2]]
  colLocsNums <- which(colLocs)
  
  ######## find elements to modify ######## 
  
  
  
  ########          ########
  ######## function ######## 

  # write function here
  
  #######MAKE SOME GGPLOTS################
  # browser()
  plotDF <- DF_in[rowLocsNums,c(1,colLocsNums)]
  
  plotTmpWtr <- tidyr::gather(plotDF,var, value, 2:ncol(plotDF))
  
 # plotTmpWtr <- lngDF[grep("TmpWtr", lngDF$var), ]
  plotTmpWtr$month <- lubridate::month(plotTmpWtr$DateTime)
  
  plotTmpWtr$hydroyr <- ifelse(plotTmpWtr$month < 7,
                               lubridate::year(plotTmpWtr$DateTime),
                               lubridate::year(plotTmpWtr$DateTime) +1)
  
  ggplot2::ggplot(plotTmpWtr, ggplot2::aes(x = DateTime, y = value)) +
    ggplot2::geom_line(ggplot2::aes(color = var)) +
#    ggplot2::scale_y_continuous(limits = c(5,35)) +
    ggplot2::ylab("Temperature (degC)") +
    ggplot2::xlab("Date") +
    ggplot2::facet_wrap(~hydroyr, ncol = 1, scales = 'free_x')
  
  ggplot2::ggsave(paste0(plotPath, metaD$lakeName,"_TmpWtr.png"), height = 30, width = 15, dpi = 400)
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