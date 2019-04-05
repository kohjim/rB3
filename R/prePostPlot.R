#' gg_facetVar
#'
#' This function plots variables on a faceted ggplot
#'
#' @export
#' @param rB3in rB3 object input
#' @param startDate measurements before this will be excluded from plot
#' @param endDate measurements after this will be excluded from plot
#' @param varNames variable names or keywords
#' @param plotLabels vector of titles for each panel NB: n and order of plotLabels must be same as columns for rB3in
#' @param srcColour colour of the unmodified data (leave out to plot only quality controlled data)
#' @param qcColour colour of the quality controlled data (leave out to plot only unmodified/raw data)
#' @param showPlot display figure in plots window (TRUE/FALSE)
#' @param savePlot save figure to this path ('folder/start_of_file_name')
#' @keywords customFun
#' @examples newDF <- gg_facetVar(rB3in = stdDF, metaD = metaD, varNames = wqVars, srcColour = 'red',
#'               qcColour = 'black', savePlot = 'figures/RAW_WQ_',  dpi = 400)
#'
prePostPlot <- function(rB3in, startDate, endDate, varNames, plotLabels, srcColour, hlColour, qcColour, showPlot, savePlot, dpi) {

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

  if (missing(qcColour)){
    lineColour <- NULL
  }

  if (missing(srcColour)){
    srcColour <- NULL
  }

  if (missing(hlColour)){
    hlColour <- NULL
  }

  if (missing(qcColour)){
    qcColour <- NULL
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


  # check for valid plotLabels and replace with varName if necessary

  for (n in 1:length(plotLabels)){

    if (is.na(plotLabels[n])) {
      colnames(rB3in[["qcDF"]])[n + 1]
    } else {
      plotLabels[n]
    }

  }


  ####### MAKE A FACETED GGPLOT ################

  plotQC <- rB3in[["qcDF"]][,c(1,colLocsNums)]
  colnames(plotQC) <- c("DateTime",plotLabels[colLocsNums - 1])
  plotQC <- tidyr::gather(plotQC,var, qc, 2:ncol(plotQC))


  plotHl <- rB3in[["hlDF"]][,c(1,colLocsNums)]
  colnames(plotHl) <- c("DateTime",plotLabels[colLocsNums - 1])
  plotHl <- tidyr::gather(plotHl,var, hl, 2:ncol(plotHl))


  plotSrc <- rB3in[["srcDF"]][,c(1,colLocsNums)]
  colnames(plotSrc) <- c("DateTime",plotLabels[colLocsNums - 1])
  plotSrc <- tidyr::gather(plotSrc,var, src, 2:ncol(plotSrc))

  # plotAll <- plotQC
  # plotAll$hl <- plotHl$hl
  # plotAll$src <- plotSrc$src


  ### aggregating for faster plots

  aggTimestep =
    floor(
      ((as.numeric(tail(plotQC[,1], n=1)) - as.numeric(plotQC[1,1])) / 1280) /
        (as.numeric(plotQC[2,1]) - as.numeric(plotQC[1,1]))) *
    (as.numeric(plotQC[2,1]) - as.numeric(plotQC[1,1]))

  plotQC_agg <- aggTS(
    dataIn = plotQC,
    timestep = aggTimestep / 60,
    FUN = "mean",
    pullAgg = "left",
    outType = "LF"
  )

  plotHl_agg <- aggTS(
    dataIn = plotHl,
    timestep = aggTimestep / 60,
    FUN = "mean",
    pullAgg = "left",
    outType = "LF"
  )

  plotSrc_agg <- aggTS(
    dataIn = plotSrc,
    timestep = aggTimestep / 60,
    FUN = "mean",
    pullAgg = "left",
    outType = "LF"
  )


  plotAll <- plotQC_agg
  colnames(plotAll) = c("DateTime","var","qc")
  plotAll$hl <- plotHl_agg[,3]
  plotAll$src <- plotSrc_agg[,3]
  ###

  # generate plot
  print(

    ggplot2::ggplot(plotAll) +

      ggplot2::geom_line(ggplot2::aes(x = DateTime, y = src, color = "Unmodified source data"), size = 0.2, na.rm = T) +
      ggplot2::geom_line(ggplot2::aes(x = DateTime, y = qc, color = "QC data"), size = 0.2, na.rm = T) +
      ggplot2::geom_line(ggplot2::aes(x = DateTime, y = hl, color = "Data to be modified"), size = 0.2, na.rm = T) +

      ggplot2::ylab("Value") +
      ggplot2::xlab(NULL) +
      ggplot2::scale_x_datetime(labels = scales::date_format("%Y-%m"),
                                # breaks = scales::date_breaks("1 years"),
                                limits = c(min(plotAll$DateTime),max(plotAll$DateTime)),
                                expand = c(0, 0)) +
      ggplot2::scale_colour_manual("",values = c("Unmodified source data"=srcColour,
                                                 "QC data" = qcColour,
                                                 "Data to be modified"=hlColour)) +

      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 0, hjust = 0)) +
      ggplot2::theme_bw() +
      ggplot2::theme(legend.position = "bottom") +
      ggplot2::facet_wrap(~var, ncol = 1, scales = 'free_y')

  )

}
######## end function ########
