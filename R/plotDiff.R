#' Sub-module to plot two timeseries affected by the changes
#'
#' Sub-module to plot two timeseries (called from modules)
#' 
#' @export
#' @param DF_pre data frame 1
#' @param DF_post data frame 2
#' @param metaD metadata list
#' @param varNames list of variable names or keywords to plot
#' @param colNum column number to plot
#' @param plotPath path of the images to be saved
#' @param isScatter if TRUE, plot scatter with lines
#' @keywords fileIO
#' @examples newDF <- exclude_vars(myDF,metaData,varNames = c("pH","wndDir"))
#' 
#' 

plotDiff <- function(DF_pre, DF_post, varNames, colNum, plotPath, custom_dpi, taskName, isScatter){
  
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
  
  if (missing(isScatter)){
    isScatter <- FALSE
  }
  ########## end defaults ##########
  
  ########## function ##########
  
  if (length(colNum) > 0){
    
    for (i in colNum){
      thisColLoc <- i
      
      thisColName <- colnames(DF_pre[i])
      DF_plot <- DF_pre[,c(1,i)]
      
      # open file
      png(filename = paste0(plotPath,taskName,"_",i,"_",thisColName,".png"),
          width = 1500, 
          height = 1000,
          res = custom_dpi)
      
      # par(mar=c(1,1,1,1))
      
      # plot DF1
      if (!isScatter){
        
        plot(DF_plot[,1:2],
             type = "l",
             col = "black")
        
      } else {
        
        # scatter with line plot (slow)
        plot(DF_plot[,1:2],
             type = "o",
             col = "black",
             pch = 0,
             cex = 0.1)
        
      }
      
      par(new = T) # second plot is going to get added to first
      
      tryCatch({
        ColLoc_out <- which(colnames(DF_post)==thisColName)
        DF_plot2 <- DF_post[,c(1,ColLoc_out)]
        
        # transparent red
        tpRed <- rgb(1,0,0,alpha=0.75) 
        
        
        # plot DF2
        if (!isScatter){
          
          plot(DF_plot2,
               col = tpRed,
               type = "l",
               axes = FALSE)
          
        } else {
          
          # scatter with line (slow)
          plot(DF_plot2,
               col = tpRed,
               type = "l",
               pch = 1,
               cex = 0.1,
               axes = FALSE)
          
        }
      }, error = function(e) {
        
      })
      
      # close file and save the image
      dev.off() 
    }
  }
  ########## end function ##########
  
}