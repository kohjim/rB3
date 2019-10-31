#' Produce a standard rB3 format ggplot
#'
#' @export
#' @param df_in data frame input, must be long format with DateTime, var, value and src
#' @param dateStart start date
#' @param dateEnd end date
#' @param siteName name of site for saving plot
#' @param colours vector of colours to use when plotting (qc then raw), or choose 'auto'
#' @param facet facet the plot by variable (TRUE/FALSE)
#' @param showPlot display figure in plots window (TRUE/FALSE)
#' @param savePlot save figure to this path ('folder/start_of_file_name')
#' @param dpi dpts per inch for saved plot
#' @keywords plotting
#' @examples rB3plotr(df_in, dateStart = '2015-07-01', dateEnd = '2016-06-30')
#'
rB3plotr <- function(df_in, siteName, dateStart, dateEnd, colours, facet, showPlot, savePlot, dpi) {

######## set defaults ########

if (missing(dateStart)){
  dateStart <- min(df_in$DateTime)
}

if (missing(dateEnd)){
  dateEnd <- max(df_in$DateTime)
}

if (missing(siteName)){
  siteName <- 'unknown_site'
}

  if (missing(colours)){
    colours <- 'auto'
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

df_plot <- df_in

# define the basic plot
varPlot <-
  ggplot2::ggplot(df_plot) +
  ggplot2::labs(x = NULL, y = NULL, colour = NULL) +
  ggplot2::scale_x_datetime(labels = scales::date_format("%Y-%m"),
                            # breaks = scales::date_breaks("1 years"),
                            limits = c(min(df_plot$DateTime),max(df_plot$DateTime)),
                            expand = c(0, 0)) +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 0, hjust = 0)) +
  ggplot2::theme_bw() +
  ggplot2::theme(legend.position = "bottom") +
  ggplot2::theme(text=ggplot2::element_text(family="serif")) +
  # remove grid
  ggplot2::theme(panel.grid.major = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank()) +
  ggplot2::theme(legend.position="bottom")#,legend.background = ggplot2::element_rect(linetype = 1, size = 0.5, colour = 'grey')  )

# facetting
if (facet == TRUE) {

  varPlot <- varPlot +
    ggplot2::geom_line(ggplot2::aes(x = DateTime, y = value, color = src), size = 0.2) +
    ggplot2::facet_wrap(~var, ncol = 1, scales = 'free_y') +
    # facet labels
    ggplot2::theme(strip.text = ggplot2::element_text(hjust = 0)) +
    ggplot2::theme(strip.background = ggplot2::element_rect(colour= 'white', fill= 'white'))

  # autoscale saving dimensions
  saveHeight = 0.5 + 1.1 * length(unique(df_plot$var))


} else {

  varPlot <- varPlot +
    ggplot2::geom_line(ggplot2::aes(x = DateTime, y = value, color = var), size = 0.2)

  saveHeight = 3

}

# fill out colours list with black, in case not long enough
if (colours[1] != 'auto') {
  varPlot <- varPlot +
    ggplot2::scale_colour_manual(values = c(colours,rep('grey',50)))
}

if (showPlot == TRUE) {

  print(varPlot)

  if (!is.null(savePlot)) {

    ggplot2::ggsave(paste0(savePlot, '_', siteName,".png"),
                    height = saveHeight,
                    width = 7.5,
                    dpi = dpi)
  }

 }

}
