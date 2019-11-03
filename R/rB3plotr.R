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
#' @param geom 'line' or 'dot' (default line)
#' @param facet facet the plot by variable (TRUE/FALSE)
#' @param showPlot display figure in plots window (TRUE/FALSE)
#' @param savePlot save figure to this path ('folder/start_of_file_name')
#' @param dpi dots per inch for saved plot
#' @keywords plotting
#' @examples rB3plotr(dt_in, siteName = 'Test_site', savePlot = 'figures/testPlot')
#'
rB3plotr <- function(dt_in, siteName, cols.qc, cols.src, cols.hl, geom,  facet, showPlot, savePlot, dpi) { # dateStart, dateEnd,

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
    cols.qc <- 'auto'
  }

  if (missing(cols.src)){
    cols.src <- 'auto'
  }

  if (missing(cols.hl)){
    cols.hl <- 'auto'
  }

  if (missing(geom)){
    geom <- 'line'
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
  plotSrc <- FALSE
  if ('src' %in% names(plotDF) == TRUE) {plotSrc <- TRUE}
  plotHl <- FALSE
  if ('hl' %in% names(plotDF)  == TRUE) {plotHl <- TRUE}

# define path for saving plot
  plotPath <- paste0(savePlot, '_', siteName,".png")

# number of variables for plotting
varLength <- length(unique(dt_in$var))

  qcData <- dt_in
  #### define plotting colours

  # if not enough colours, then use first only
  if (cols.qc == 'auto') {

    cols.qc <- unname(randomcoloR::distinctColorPalette(varLength))

    # set colour label for legend
    plotDF$col <- plotDF$var

  } else if (length(cols.qc) < length(varLength) {

    print('Not enough colours provided for QC data, using first colour only')

  # pad out to length of vars plotted
  cols.qc <- cols.qc[1]

  # set colour label for legend
  plotDF$col <- "Quality controlled data"

    # otherwise use individual colours
  } else {

    cols.qc <- cols.qc[1: length(unique(plotDF$var) )]
    # set colour label for legend
    plotDF$col <- plotDF$var

  }

  # set final colours to modified cols.qc
  cols <- cols.qc



  ##### add source data if specified
  if (plotSrc == TRUE) {

    srcData <- plotDF[,c("DateTime","var","src","col")]
      names(srcData) <- c('DateTime','var','value','col')


  ## define colours
  if (cols.src == 'auto') {

    #..lighten the qc colours to use as src
    cols.src <- colorspace::lighten(cols, 0.5, fixup = T)

    # set colour label for legend
    srcData$col <- ifelse( length(unique(plotDF$col)) > 1,
                           paste0(srcData$var,'.raw'),
                           "Unmodified/source data" )

  } else if (length(cols.src) < length(unique(srcData$var)) ) {

    print('Not enough colours provided for source/raw data, using first colour only')

    # pad out to length of vars plotted
    cols.src <- cols.src[1]

    # set colour label for legend
    srcData$col <- "Unmodified/source data"

    # otherwise use individual colours
  } else {

    cols.src <- cols.src[1: length(unique(srcData$var) )]
    # set colour label for legend
    srcData$col <- srcData$var

  }

      # add the source/raw colours to the vector
      cols <- c(cols, cols.src)


      ### BIND the qc and src data together for plotting
      plotDF <- rbind(plotDF[,c("DateTime","var","value","col")],srcData)

# end src data add
}


  ##### add highlighting data if specified
  if (plotHl == TRUE) {

    # trim to original data length, keep cols needed
    hlData <- dt_in[,c("DateTime","var","hl","col")]
       names(hlData) <- c('DateTime','var','value','col')

       ## define colours
       if (cols.hl == 'auto') {

         #..set to red
         cols.hl <- 'red'

         # set colour label for legend
         hlData$col <- 'Data to be modified'

       } else if (length(cols.hl) < length(unique(hlData$var)) ) {

         print('Not enough colours provided for highlighting data, using first colour only')

         # pad out to length of vars plotted
         cols.hl <- cols.hl[1]

         # set colour label for legend
         hlData$col <- "Data to be modified"

         # otherwise use individual colours
       } else {

         cols.hl <- cols.hl[1: length(unique(hlData$var) )]
         # set colour label for legend
         hlData$col <- paste0(hlData$var,'.mod')

       }

       # add the highlighting colours to the vector
       # cols <- c(matrix(c(cols, cols.hl), 3, byrow = T))
       cols <- c(cols, cols.hl)

       ### BIND the qc and src data together for plotting
       plotDF <- rbind(plotDF,hlData)


    }


# set geometry

if (geom == 'dot') {

  geom <- ggplot2::geom_point(ggplot2::aes(x = DateTime, y = value, color = col), size = 0.3)

} else { geom <- ggplot2::geom_line(ggplot2::aes(x = DateTime, y = value, color = col), size = 0.5) }



  # define the basic plot
varPlot <-
ggplot2::ggplot(plotDF) +
  geom +
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

  saveHeight = 4

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


