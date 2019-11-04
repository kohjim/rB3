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
    cols.qc <- 'blue'
  }

  if (missing(cols.src)){
    cols.src <- 'grey'
  }

  if (missing(cols.hl)){
    cols.hl <- 'red'
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

  # define path for saving plot
  plotPath <- paste0(savePlot, '_', siteName,".png")

  # number of variables for plotting
  varLength <- length(unique(dt_in$var))

  # initialise colours
  cols <- NULL

  # intialise DT
  plotDT <- dt_in[0,c(1:3)]




  #### find plotting colours in the correct order

  ## find qc colours
  if (cols.qc == 'auto') {

    cols.qc <- unname(randomcoloR::distinctColorPalette(varLength))

  } else if (length(cols.qc) < varLength ) {

    # use first
    cols.qc <- cols.qc[1]

  } else if (length(cols.qc) >= varLength ) {

    cols.qc <- cols.qc[1: varLength]

  }


  ## find src colours if auto
  if (cols.src == 'auto') {

    #..lighten the qc colours to use as src
    cols.src <- colorspace::lighten(cols.qc, 0.5, fixup = T)

  } else if (length(cols.src) < varLength ) {

    # use first
    cols.src <- cols.src[1]

  } else if (length(cols.qc) >= varLength ) {

    cols.src <- cols.src[1: varLength]

  }

  # set hl cols to red if auto
  if (cols.hl == 'auto') {

    #..set to red
    cols.hl <- 'red'

  } else if (length(cols.hl) > 0 ) {

    # use first
    cols.hl <- cols.hl[1]

  }

  # combine into one
  cols <- NULL



  ##### RAW/SRC DATA
  if ('src' %in% names(dt_in) == TRUE) {

    srcData <- dt_in[,c("DateTime","var","src")]
    names(srcData) <- c('DateTime','var','value')

    ## set colour legend
    ## set colour legend
    if (length(cols.src) > 1) {

      srcData$col <- paste0(srcData$var,'.raw')

    } else {

      srcData$col <- 'Unmodified/source data'

    }

    ### BIND the qc and src data together for plotting
    plotDT <- rbind(plotDT,srcData)

    cols <- c(cols,cols.src)
    # end src data add
  }


  ##### QC DATA
  if ('qc' %in% names(dt_in) == TRUE) {

    qcData <- dt_in[,c("DateTime","var","qc")]
    names(qcData) <- c('DateTime','var','value')

    ## set colour legend
    if (length(cols.qc) > 1) {

      qcData$col <- qcData$var

    } else {

      qcData$col <- 'Quality controlled data'

    }


    ### BIND the qc and src data together for plotting
    plotDT <- rbind(plotDT,qcData)

    cols <- c(cols,cols.qc)
    # end qc data add
  }

  ##### Highlighting DATA
  if ('hl' %in% names(dt_in) == TRUE) {

    hlData <- dt_in[,c("DateTime","var","hl")]
    names(hlData) <- c('DateTime','var','value')

    ## set colour legend
    hlData$col <- 'Data to be modified'

    ### BIND the qc and src data together for plotting
    plotDT <- rbind(plotDT,qcData)

    cols <- c(cols,cols.hl)
    # end hl data add
  }

  # ensure correct colour order
  plotDT$col <- factor(plotDT$col, levels = unique(plotDT$col))


  # set geometry
  if (geom == 'dot') {

    geom2use <- ggplot2::geom_point(ggplot2::aes(x = DateTime, y = value, color = col), size = 0.3)

  } else { geom2use <- ggplot2::geom_line(ggplot2::aes(x = DateTime, y = value, color = col), size = 0.3) }



  # define the basic plot
  varPlot <-
    ggplot2::ggplot(plotDT) +
    geom2use +
    ggplot2::scale_colour_manual(values = cols) +
    ggplot2::scale_x_datetime(#labels = scales::date_format("%Y-%m"),
      #limits = c(min(dt_in$DateTime),max(dt_in$DateTime)),
      expand = c(0, 0)) +
    ggplot2::labs(x = NULL, y = NULL, color = NULL) +
    # ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 0, hjust = 0)) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "bottom") +
    ggplot2::theme(text=ggplot2::element_text(family="serif", size = 16)) +
    # remove grid
    ggplot2::theme(panel.grid.major = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank()) +
    # facet labels
    ggplot2::theme(strip.text = ggplot2::element_text(hjust = 0)) +
    ggplot2::theme(strip.background = ggplot2::element_rect(colour= 'white', fill= 'white'))

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


