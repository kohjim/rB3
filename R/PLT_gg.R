#' Plot source and/or quality controlled data
#'
#' Produce a simple, or faceted, ggplot of variable(s)
#'
#' @export
#' @param dataIn data table, data frame, or rB3 object input
#' @param dateRange vector of start and end DateTime to include
#' @param varNames variable names or keywords to detect variables by
#' @param plotLabels vector of full names for all variables (in order)
#' @param cols.src colour of the unmodified data (leave out to plot only quality controlled data)
#' @param cols.qc colour of the quality controlled data (leave out to plot only unmodified/raw data)
#' @param facet facet the plot by variable (TRUE/FALSE)
#' @param geom 'dot' or 'line' plot type
#' @param showPlot display figure in plots window (TRUE/FALSE)
#' @param savePlot save figure to this path ('folder/start_of_file_name')
#' @keywords customFun
#' @examples PLT_gg <- rB3gg(dataIn, dateRange, varNames, cols.qc, cols.src, facet,
#'                            geom, showPlot, siteName, savePlot, dpi)
#'
PLT_gg <- function(dataIn, dateRange, varNames, cols.qc, cols.src, facet, geom, showPlot, savePlot, dpi) {

  ######## set defaults ########
  if (missing(dateRange)){
    dateRange <- c(NA,NA)
  }

  if (missing(varNames)){
    varNames <- "All"
  }

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

  # if (missing(siteName)){
  #   siteName <- 'Unknown_site'
  # }

  if (missing(savePlot)){
    savePlot <- NULL
  }

  if (missing(dpi)){
    dpi <- 300
  }

  ######## end set defaults ########

  ######## find rows & cols to plot ########
  IDels <- .WRG_IDel(dataIn,
                     dateRange = dateRange,
                     varNames = varNames)

  # decompose the list
  rowLocsNums <- IDels[[1]]
  colLocsNums <- IDels[[2]]


  # if (missing(plotLabels)){
  #   plotLabels <- colnames(dt.in[,colLocsNums])
  # }


  ####### MAKE A FACETED GGPLOT ################

  # plotting data
  plotData <- as.data.frame(dataIn)
  plotData <- plotData[rowLocsNums,c(1,colLocsNums)]
  plotDF <- .AGG_plotData(plotData)
    names(plotDF) <- c("DateTime","var","qc")

  # if (!is.null(cols.qc)) {
  #
  #   plotDF <- plotQC
  #
  # } else {
  #
  #   plotDF <- plotQC$DateTime
  #
  # }
  #
  # # add src data
  # if (!is.null(cols.src)) {
  # plotSrc <- dataIn[["srcDF"]][rowLocsNums,c(1,colLocsNums)]
  # # colnames(plotQC) <- c("DateTime",plotLabels[colLocsNums - 1])
  # plotSrc <- aggPlotData(plotSrc)
  #    names(plotSrc) <- c("DateTime","var","src")
  #
  #
  # plotDF <- cbind(plotDF, plotSrc[,3,drop = F])
  #   names(plotSrc) <- c("DateTime","var","src")
  #
  # }

  .PLT_plotr(plotDF,  cols.qc = cols.qc, # cols.src = cols.src, siteName = siteName,
           geom = geom,  facet = facet, showPlot = showPlot, savePlot = savePlot, dpi = dpi)

}
  ######## end function ########
