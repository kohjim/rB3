#' Sub-module to plot two timeseries affected by the changes
#'
#' Sub-funciton to plot two timeseries (called from modules)
#' 
#' @export
#' @param DF_pre data frame 1
#' @param DF_post data frame 2
#' @param metaD metadata list
#' @param varNames list of variable names or keywords to plot
#' @param colNum column number to plot
#' @param plotPath path of the images to be saved
#' @keywords fileIO
#' @examples newDF <- exclude_vars(myDF,metaData,varNames = c("pH","wndDir"))
#' 
#' 

plotDiff <- function(DF_pre, DF_post, varNames, colNum, plotPath, custom_dpi, taskName){
  
  ########## defaults ##########
  if (missing(varNames) & missing(colNum)){
    colNum <- c(2:length(DF_pre))
  } else if (missing(colNum)){
    outs.idElToModify <- idElToModify(DF_pre, varNames = varNames)
    colLocs <- outs.idElToModify[[2]]
    colNum <- which(colLocs)
  }
  
  if (missing(custom_dpi)){
    custom_dpi <- 150
  }
  
  if (missing(plotPath)){
    plotPath <- "plotDiff"
  }
  
  if (missing(taskName)){
    taskName <- NULL
  }
  ########## end defaults ##########
  
  ########## function ##########

  if (length(colNum) > 0){
    
    # browser()
    for (i in colNum){
      thisColLoc <- colNum[i]
      thisColName <- colnames(DF_pre[i])
      
      DF_plot <- DF_pre[,c(1,i)]
      DF_plot$gp <- "in"
      
      tryCatch({
        ColLoc_out <- which(colnames(DF_post)==thisColName)
        thisDF_out <- DF_post[,c(1,ColLoc_out)]
        thisDF_out$gp <- "out"
        
        DF_plot <- rbind(DF_plot,thisDF_out)
        
      }, error = function(e) {
        
      })
      
      f1 <- ggplot2::ggplot(DF_plot, ggplot2::aes(x = DF_plot[,1],y = DF_plot[,2]))
      f1  + ggplot2::geom_line(ggplot2::aes(color = gp),size = 0.1)
      
      ggplot2::ggsave(
        paste0(plotPath,taskName,"_",i,"_",thisColName,".png"),
        dpi = custom_dpi
      )
    }
  }
  ########## end function ##########
  
}