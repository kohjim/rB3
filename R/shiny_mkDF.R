shiny_mkDF <- function(rB3in, startDate, endDate, varNames, isPlotSrc){

  plotLabels <- rB3in[["ctrls"]]$plotLabels
  
  ######## find elements to modify ########
  outs.idElToModify <- idElToModify(
    rB3in,
    startDate = startDate,
    endDate = endDate,
    varNames = varNames)
  # decompose the list
  rowLocs <- outs.idElToModify[[1]]
  rowLocsNums <- which(rowLocs)
  colLocs <- outs.idElToModify[[2]]
  colLocsNums <- which(colLocs)
  
  ####### MAKE A FACETED GGPLOT ################
  
  plotQC <- rB3in[["qcDF"]][rowLocsNums,c(1,colLocsNums)]
  colnames(plotQC) <- c("DateTime",plotLabels[colLocsNums - 1])
  plotAll <- tidyr::gather(plotQC,var, qc, 2:ncol(plotQC))
  
  if (isPlotSrc) {
    plotSrc <- rB3in[["srcDF"]][rowLocsNums,c(1,colLocsNums)]
    colnames(plotSrc) <- c("DateTime",plotLabels[colLocsNums - 1])
    plotSrc <- tidyr::gather(plotSrc,var, value, 2:ncol(plotSrc))
    
    plotAll$src <- plotSrc$value
    
  } else { 
    
    plotAll$src <- NA
  }
  
  return(plotAll)
}