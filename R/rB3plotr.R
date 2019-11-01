#' Produce a standard rB3 format ggplot
#'
#' @export
#' @param dt_in data input, must be long format with DateTime, var, value and (optionally) src and/or hl
#' @param plotSrc plot source/raw data as well as qc data
#' @param plotHl plot highlighting
#' param dateStart start date
#' param dateEnd end date
#' @param siteName name of site for saving plot
#' @param cols.qc vector of colours to use when plotting qc data, or choose 'auto'
#' @param cols.src vector of colours to use when plotting source/raw data (default = 'light grey')
#' @param facet facet the plot by variable (TRUE/FALSE)
#' @param showPlot display figure in plots window (TRUE/FALSE)
#' @param savePlot save figure to this path ('folder/start_of_file_name')
#' @param dpi dots per inch for saved plot
#' @keywords plotting
#' @examples rB3plotr(dt_in, siteName = 'Test_site', savePlot = 'figures/testPlot')
#'
rB3plotr <- function(dt_in, siteName, cols.qc, cols.src, cols.hl, facet, showPlot, savePlot, dpi) { # dateStart, dateEnd,

######## set defaults ########

# if (missing(dateStart)){
#   dateStart <- min(df_in$DateTime)
# }
#
# if (missing(dateEnd)){
#   dateEnd <- max(df_in$DateTime)
# }

  ## need to make this pull site from metaD if it exists
if (missing(siteName)){
  siteName <- 'unknown_site'
}

  if (missing(cols.qc)){
    cols.qc <- 'blue'
  }

  if (missing(cols.src)){
    cols.src <- 'lightgrey'
  }

  if (missing(cols.hl)){
    cols.hl <- 'red'
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
    dpi <- 600
  }

####### MAKE FACETED GGPLOT ################

  # scan headings to see which ones to plot..
  plotDF <- dt_in

  if ('src' %in% names(plotDF) == T) {plotSrc == T}
  if ('hl' %in% names(plotDF)  == T) {plotHl == T}

# define path for saving plot
  plotPath <- paste0(savePlot, '_', siteName,".png")


  #### define plotting colours

  # if not enough colours, then use first only
  if (cols.qc == 'auto' & plotHl != T) {

    cols.qc <- unname(randomcoloR::distinctColorPalette(25))

    # set colour label for legend
    plotDF$col <- ifelse(plotHl == TRUE, "Quality controlled data", plotDF$var)

  } else if (length(cols.qc) < length(unique(plotDF$var)) ) {

    print('Not enough colours provided for QC data, using first colour only')

  # set colour label for legend
  plotDF$col <- "Quality controlled data"

  # pad out to length of vars plotted
  cols.qc <- rep(cols.qc[1], length(unique(plotDF$var) ) )

    # otherwise use individual colours
  } else {

    cols.qc <- cols.qc[1: length(unique(plotDF$var) )]
    # set colour label for legend
    plotDF$col <- ifelse(plotHl == TRUE, "Quality controlled data", plotDF$var)

  }

  # set final colours to modified cols.qc
  cols <- col.qc



  ##### add source data if specified
  if (plotSrc == TRUE) {

    srcData <- plotDF[,c(1:2,"src","col")]
      names(srcData) <- c('DateTime','var','value','col')

    #### set src colours to light versions of qc colours, unless specifically specified

    # if not enough colours for src provided..
    if (length(cols.src) < length(unique(plotDF$var)) ) {

      print('Not enough colours provided for source/raw data, choosing them for you')

      # set colour label for legend
      srcData$col <- "Source/raw data"

      # set colour label for legend
      srcData$col <- ifelse(plotHl == TRUE | length(unique(cols.qc)) == 1,
                            "Quality controlled data",
                            srcData$var)

      #..and lighten the qc colours to use as src
      cols.src <- colorspace::lighten(cols, 0.5, fixup = T)

      # pad out to length of vars plotted
      cols.src <- rep(cols.src[1], length(unique(srcData$var) ) )

      # add source/raw colours to the vector
      cols <- c(matrix(c(cols, cols.src), 2, byrow = T))

      ### BIND the qc and src data together for plotting
      plotDF <- rbind(plotDF[,c(1:3,5)],srcData)

      }



  }

  ## add source data if specified
  if (plotHl != FALSE) {

    hlData <- plotDF[,c(1:2,"hl","col")]
       names(hlData) <- c('DateTime','var','value','col')

    cols.hl <- rep('red',length())

    }

    # add the source/raw colours to the vector
    cols <- c(matrix(c(cols, cols.src), 2, byrow = T))


    ### BIND the qc and src data together for plotting
    plotDF <- rbind(plotDF[,c(1:3,5)],srcData)

  }





# define the basic plot
varPlot <-
ggplot2::ggplot(plotDF) +
  ggplot2::geom_line(ggplot2::aes(x = DateTime, y = value, color = col), size = 0.2) +
  ggplot2::scale_colour_manual(values = cols) +
  ggplot2::scale_x_datetime(#labels = scales::date_format("%Y-%m"),
                            #limits = c(min(dt_in$DateTime),max(dt_in$DateTime)),
                            expand = c(0, 0)) +
  ggplot2::labs(x = NULL, y = NULL, color = NULL) +
  # ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 0, hjust = 0)) +
  plotTheme()

# facetting
if (facet == TRUE) {

  varPlot <- varPlot +

    ggplot2::facet_wrap(~var, ncol = 1, scales = 'free_y')

  # autoscale saving dimensions
  saveHeight = 2 + 1.3 * length(unique(dt_in$var))

} else {

  saveHeight = 3

}

if (showPlot == TRUE) {

  print(varPlot)

}

  if (!is.null(savePlot)) {

    ggplot2::ggsave(plot = varPlot, plotPath,
                    height = saveHeight,
                    width = 12,
                    dpi = dpi)
  }

}


