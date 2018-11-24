#' logsPlot
#'
#' This function plots variables on a faceted ggplot
#'
#' @export
#' @param rB3in rB3 object input
#' @param startDate measurements before this will be excluded from plot
#' @param endDate measurements after this will be excluded from plot
#' @param varNames variable names or keywords
#' @param plotLabels vector of titles for each panel NB: n and order of plotLabels must be same as columns for rB3in
#' @param srcColour colour of the quality controlled data (leave out to plot only unmodified/raw data)
#' @param showPlot display figure in plots window (TRUE/FALSE)
#' @param savePlot save figure to this path ('folder/start_of_file_name')
#' @keywords customFun
#' @examples newDF <- gg_facetVar(rB3in = stdDF, metaD = metaD, varNames = wqVars, srcColour = 'red',
#'               qcColour = 'black', savePlot = 'figures/RAW_WQ_',  dpi = 400)
#'
logsPlot <- function(rB3in, startDate, endDate, varNames, plotLabels, srcColour, showPlot, savePlot, dpi) {

  ######## set defaults ########
  if (missing(startDate)){
    startDate <- rB3in[["qcDF"]]$DateTime[1]
  }

  if (missing(endDate)){
    endDate <- rB3in[["qcDF"]]$DateTime[length(rB3in[["qcDF"]]$DateTime)]
  }

  if (missing(varNames)){
    varNames$Vars <- "All"
  }

  if (missing(plotLabels)){
    plotLabels <- rB3in[["ctrls"]]$plotLabels
  }

  if (missing(srcColour)){
    lineColour <- 'grey'
  }

  if (missing(showPlot)){
    showPlot <- TRUE
  }

  if (missing(savePlot)){
    savePlot <- NULL
  }

  if (missing(dpi)){
    dpi <- 300
  }

  ######## end set defaults ########

  ######## find rows & cols to plot ########
  outs.idElToModify <- idElToModify(rB3in,
                                    startDate = startDate,
                                    endDate = endDate,
                                    varNames = varNames)
  # decompose the list
  rowLocs <- outs.idElToModify[[1]]
  rowLocsNums <- which(rowLocs)
  colLocs <- outs.idElToModify[[2]]
  colLocsNums <- which(colLocs)



  ####### MAKE A FACETED GGPLOT ################

  plotQC <- rB3in[["qcDF"]][,c(1,colLocsNums)]
  colnames(plotQC) <- c("DateTime",plotLabels[colLocsNums - 1])
  plotAll <- tidyr::gather(plotQC,var, qc, 2:ncol(plotQC))


    plotLogs <- rB3in[["logDF"]][,c(1,colLocsNums)]
    colnames(plotLogs) <- c("DateTime",plotLabels[colLocsNums - 1])
    plotLogs <- tidyr::gather(plotLogs,var, logs, 2:ncol(plotLogs))


    plotSrc <- rB3in[["srcDF"]][,c(1,colLocsNums)]
    colnames(plotSrc) <- c("DateTime",plotLabels[colLocsNums - 1])
    plotSrc <- tidyr::gather(plotSrc,var, src, 2:ncol(plotSrc))

    plotAll$logs <- plotLogs$logs
       plotAll$logs <- ifelse(is.na(plotAll$logs), "No change", plotAll$logs)

    plotAll$src <- plotSrc$src


    browser()
  varPlot <-

   ggplot2::ggplot(plotAll) +
    ggplot2::geom_line(ggplot2::aes(x = DateTime, y = src, color = logs), size = 0.2) +
    ggplot2::geom_line(ggplot2::aes(x = DateTime, y = qc, color = logs), size = 0.2) +
    ggplot2::ylab("Value") +
    ggplot2::xlab(NULL) +
    ggplot2::scale_x_datetime(labels = scales::date_format("%Y-%m"),
                     # breaks = scales::date_breaks("1 years"),
                     limits = c(min(plotAll$DateTime),max(plotAll$DateTime)),
                     expand = c(0, 0)) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 0, hjust = 0)) +
    ggplot2::facet_wrap(~var, ncol = 1, scales = 'free_y') +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "bottom")


  if (showPlot == TRUE) {

    print(varPlot)

    }

  if (!is.null(savePlot)) {

    ggplot2::ggsave(paste0(savePlot, rB3in[["metaD"]]$siteName,"_facet.png"),
                    height = 1.2 * length(unique(plotAll$var)),
                    width = 7.5,
                    dpi = dpi)
  }

}
######## end function ########
