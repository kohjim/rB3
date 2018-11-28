#' Plot source and/or quality controlled data
#'
#' Produce a simple, or faceted, ggplot of variable(s)
#'
#' @export
#' @param rB3in rB3 object input
#' @param startDate measurements before this will be excluded from plot
#' @param endDate measurements after this will be excluded from plot
#' @param varNames variable names or keywords
#' @param plotLabels vector of titles for each panel NB: n and order of plotLabels must be same as columns for rB3in
#' @param srcColour colour of the unmodified data (leave out to plot only quality controlled data)
#' @param qcColour colour of the quality controlled data (leave out to plot only unmodified/raw data)
#' @param facet facet the plot by variable (TRUE/FALSE)
#' @param showPlot display figure in plots window (TRUE/FALSE)
#' @param savePlot save figure to this path ('folder/start_of_file_name')
#' @keywords customFun
#' @examples newDF <- rB3gg(rB3in = stdDF, metaD = metaD, varNames = wqVars, srcColour = 'red',
#'               qcColour = 'black', facet = TRUE, savePlot = 'figures/RAW_WQ_',  dpi = 400)
#'
rB3gg <- function(rB3in, startDate, endDate, varNames, plotLabels, srcColour, qcColour, facet, showPlot, savePlot, dpi) {

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

  if (missing(qcColour)){
    qcColour <- NULL
  }

  if (missing(facet)){
    facet <- FALSE
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



  # check for valid plotLabels and replace with varName if necessary

  for (n in 1:length(plotLabels)){

    if (is.na(plotLabels[n])) {
      colnames(rB3in[["qcDF"]])[n + 1]
    } else {
      plotLabels[n]
    }

  }



  ####### MAKE A FACETED GGPLOT ################

  plotQC <- rB3in[["qcDF"]][rowLocsNums,c(1,colLocsNums)]
  colnames(plotQC) <- c("DateTime",plotLabels[colLocsNums - 1])
  plotAll <- tidyr::gather(plotQC,var, qc, 2:ncol(plotQC))

  if (!is.null(srcColour)) {
    plotSrc <- rB3in[["srcDF"]][rowLocsNums,c(1,colLocsNums)]
    colnames(plotSrc) <- c("DateTime",plotLabels[colLocsNums - 1])
    plotSrc <- tidyr::gather(plotSrc,var, value, 2:ncol(plotSrc))

    plotAll$src <- plotSrc$value

  } else { plotAll$src <- NA
  }

  # colours
  if (facet == TRUE) {
    srcGeom <- ggplot2::geom_line(ggplot2::aes(x = DateTime, y = src, color = "Unmodified data"), size = 0.2)
    qcGeom <- ggplot2::geom_line(ggplot2::aes(x = DateTime, y = qc, color = "Quality cotrolled data"), size = 0.2)
    srcKey <- ggplot2::scale_colour_manual("",values = c("Unmodified data"=srcColour))
    qcKey <- ggplot2::scale_colour_manual("",values = c("Quality cotrolled data"=qcColour))
    dualKey <- ggplot2::scale_colour_manual("",values = c("Unmodified data"=srcColour, "Quality cotrolled data"=qcColour))

  } else {
    srcGeom <- ggplot2::geom_line(ggplot2::aes(x = DateTime, y = src, color = var), size = 0.2)
    qcGeom <- ggplot2::geom_line(ggplot2::aes(x = DateTime, y = qc, color = var), size = 0.2)
    srcKey <- NULL
    qcKey <- NULL
    dualKey <- NULL
  }

  varPlot <-

   ggplot2::ggplot(plotAll) +
    ggplot2::ylab("Value") +
    ggplot2::xlab(NULL) +
    ggplot2::scale_x_datetime(labels = scales::date_format("%Y-%m"),
                     # breaks = scales::date_breaks("1 years"),
                     limits = c(min(plotAll$DateTime),max(plotAll$DateTime)),
                     expand = c(0, 0)) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 0, hjust = 0)) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "bottom")


  if (facet == TRUE) {

  varPlot <- varPlot +  ggplot2::facet_wrap(~var, ncol = 1, scales = 'free_y')

  }

  if (showPlot == TRUE) {

  if (is.null(srcColour)) {
   print(varPlot + qcGeom + qcKey)
  } else if ( is.null(qcColour)) {
    print(varPlot + srcGeom + srcKey)
  } else { print(varPlot + srcGeom + qcGeom + dualKey)
      }
}

if (!is.null(savePlot)) {

    ggplot2::ggsave(paste0(savePlot, rB3in[["metaD"]]$siteName,"_facet.png"),
                  height = 0.5 + 1.1 * length(unique(plotAll$var)),
                  width = 7.5,
                  dpi = dpi)
               }

}
  ######## end function ########
