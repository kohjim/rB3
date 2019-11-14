#' Produce a standard rB3 format ggplot
#'
#' @export
#' @param dt.in data input, must be long format with DateTime, var, value and (optionally) src and/or hl
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
#' @examples rB3plotr(dt.in, siteName = 'Test_site', savePlot = 'figures/testPlot')
#'
rB3plotr <- function(dt.in, siteName, cols.qc, cols.src, cols.hl, geom,  facet, showPlot, savePlot, dpi) { # dateStart, dateEnd,

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
    dpi <- 300
  }

  ####### MAKE FACETED GGPLOT ################
  # browser()
  # define path for saving plot
  plotPath <- paste0(savePlot, '_', siteName,".png")

  # number of variables for plotting
  varLength <- length(unique(dt.in$var))

  # initialise colours
  cols <- NULL


  #### find plotting colours in the correct order

  ## find qc colours
    if (length(cols.qc) >= varLength ) {

    cols.qc <- cols.qc[1: varLength]

    } else if (!is.null(cols.qc) & cols.qc[1] == 'auto') {

      # a random colour pallette: unname(randomcoloR::distinctColorPalette(30))
      cols.qc <- c("#9ECE54", "#D5934F", "#58C2D7", "#CCC8E5", "#D575E6", "#E7D73E",
                   "#5BE57B", "#D63AE4", "#7842E7", "#DFA59D", "#96EF45", "#D7DEB5",
                   "#DDD585", "#DF5F54", "#6FA483", "#7C63D3", "#ACE49A", "#78ACDF",
                   "#69E5B7", "#C640AD", "#DFAEDC", "#68E7E2", "#E0DCD6", "#E17EC0",
                   "#ABDCE9", "#CE4B7D", "#77757C", "#B0E9D4", "#6188D9", "#B691DF")

    } else if (!is.null(cols.qc)) {

      cols.qc <- cols.qc[1]

    }


    ## find src colours
    if (length(cols.src) >= varLength ) {

      cols.src <- cols.src[1: varLength]

    } else if (!is.null(cols.src) & cols.src[1] == 'auto') {

      cols.src <- colorspace::lighten(cols.qc, 0.5, fixup = T)

    } else if (!is.null(cols.src)) {

      cols.src <- cols.src[1]

    }



    # set hl cols to red if auto
    if (length(cols.hl) >= varLength ) {

      cols.hl <- cols.hl[1: varLength]

    } else {

     #..set to red
      cols.hl <- 'red'

    }

  # combine into one
  cols <- c(cols.src, cols.qc, cols.hl)

  ## blank data table for initialising components
  blank.dt <- cbind(dt.in[0,c(1:2)], matrix(nrow = 0, ncol = 2) )
      names(blank.dt) <- c("DateTime","var","value","col")


  ##### RAW/SRC DATA
  # intialise DT
  srcData <- blank.dt

  if ('src' %in% names(dt.in) == TRUE) {

    srcData <- dt.in[,c("DateTime","var","src")]
    names(srcData) <- c('DateTime','var','value')

    ## set colour legend
    if (length(cols.src) > 1) {

      srcData$col <- paste0(srcData$var,'.raw')

    } else {

      srcData$col <- 'Unmodified/source data'

    }

    # end src data add
  }


  ##### QC DATA
  # intialise DT
  qcData <- blank.dt

  if ('qc' %in% names(dt.in) == TRUE) {

    qcData <- dt.in[,c("DateTime","var","qc")]
    names(qcData) <- c('DateTime','var','value')

    ## set colour legend
    if (length(cols.qc) > 1) {

      qcData$col <- qcData$var

    } else {

      qcData$col <- 'Quality controlled data'

    }

    # end qc data add
  }

  ##### Highlighting DATA
  # intialise DT
  hlData <- blank.dt

  if ('hl' %in% names(dt.in) == TRUE) {

    hlData <- dt.in[,c("DateTime","var","hl")]
       names(hlData) <- c('DateTime','var','value')

    ## set colour legend
    hlData$col <- 'Data to be modified'

    # end hl data add
  }

  plotDT <- rbind(srcData, qcData, hlData)

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
      #limits = c(min(dt.in$DateTime),max(dt.in$DateTime)),
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
    saveHeight = 2 + 1.3 * length(unique(dt.in$var))

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


