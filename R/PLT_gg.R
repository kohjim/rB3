#' Plot source and/or quality controlled data
#'
#' Produce a simple, or faceted, ggplot of variable(s)
#'
#' @export
#' @param dataIn rB3 object input
#' @param startDate measurements before this will be excluded from plot
#' @param endDate measurements after this will be excluded from plot
#' @param varNames variable names or keywords
#' @param plotLabels vector of titles for each panel NB: n and order of plotLabels must be same as columns for rB3in
#' @param cols.src colour of the unmodified data (leave out to plot only quality controlled data)
#' @param cols.qc colour of the quality controlled data (leave out to plot only unmodified/raw data)
#' @param facet facet the plot by variable (TRUE/FALSE)
#' @param showPlot display figure in plots window (TRUE/FALSE)
#' @param savePlot save figure to this path ('folder/start_of_file_name')
#' @keywords customFun
#' @examples newDF <- rB3gg(rB3in = stdDF, metaD = metaD, varNames = wqVars, srcColour = 'red',
#'               qcColour = 'black', facet = TRUE, savePlot = 'figures/RAW_WQ_',  dpi = 400)
#'
PLT_gg <- function(dataIn, startDate, endDate, varNames, cols.qc, cols.src, facet, geom, showPlot, siteName, savePlot, dpi) {

  ######## set defaults ########
  if (missing(startDate)){
    startDate <- dataIn[["qcDF"]]$DateTime[1]
  }

  if (missing(endDate)){
    endDate <- dataIn[["qcDF"]]$DateTime[length(dataIn[["qcDF"]]$DateTime)]
  }

  if (missing(varNames)){
    varNames <- "All"
  }

  # if (missing(plotLabels)){
  #   plotLabels <- dataIn[["ctrls"]]$plotLabels
  # }

  if (missing(cols.qc)){
    cols.qc <- NULL
  }

  if (missing(cols.src)){
    cols.src <- NULL
  }

  if (missing(facet)){
    facet <- FALSE
  }

  if (missing(geom)){
    geom <- 'line'
  }

  if (missing(showPlot)){
    showPlot <- TRUE
  }

  if (missing(siteName)){
    siteName <- 'Unknown_site'
  }

  if (missing(savePlot)){
    savePlot <- NULL
  }

  if (missing(dpi)){
    dpi <- 300
  }

  ######## end set defaults ########

  ######## find rows & cols to plot ########
  outs.idElToModify <- idElToModify(dataIn,
                                    startDate = startDate,
                                    endDate = endDate,
                                    varNames = varNames)

  # decompose the list
  rowLocs <- outs.idElToModify[[1]]
  rowLocsNums <- which(rowLocs)
  colLocs <- outs.idElToModify[[2]]
  colLocsNums <- which(colLocs)



  # check for valid plotLabels and replace with varName if necessary

  # for (n in 1:length(plotLabels)){
  #
  #   if (is.na(plotLabels[n])) {
  #     colnames(rB3in[["qcDF"]])[n + 1]
  #   } else {
  #     plotLabels[n]
  #   }
  #
  # }
  #


  ####### MAKE A FACETED GGPLOT ################

  # quality control data
  plotQC <- dataIn[["qcDF"]][rowLocsNums,c(1,colLocsNums)]
     # colnames(plotQC) <- c("DateTime",plotLabels[colLocsNums - 1])
  plotQC <- aggPlotData(plotQC)
    names(plotQC) <- c("DateTime","var","qc")

  if (!is.null(cols.qc)) {

    plotDF <- plotQC

  } else {

    plotDF <- plotQC$DateTime

  }

  # add src data
  if (!is.null(cols.src)) {
  plotSrc <- dataIn[["srcDF"]][rowLocsNums,c(1,colLocsNums)]
  # colnames(plotQC) <- c("DateTime",plotLabels[colLocsNums - 1])
  plotSrc <- aggPlotData(plotSrc)
     names(plotSrc) <- c("DateTime","var","src")


  plotDF <- cbind(plotDF, plotSrc[,3,drop = F])
    names(plotSrc) <- c("DateTime","var","src")

  }

  rB3plotr(plotDF, siteName = siteName, cols.qc = cols.qc, cols.src = cols.src,
           geom = geom,  facet = facet, showPlot = showPlot, savePlot = savePlot, dpi = dpi)

}
  ######## end function ########
