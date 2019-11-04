# ggplot themes common to all plots

plotTheme <- function() {

  plotTheme <-
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "bottom") +
    ggplot2::theme(text=ggplot2::element_text(family="serif", size = 18)) +
    # remove grid
    ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
                   panel.grid.minor = ggplot2::element_blank()) +
    # facet labels
    ggplot2::theme(strip.text = ggplot2::element_text(hjust = 0)) +
    ggplot2::theme(strip.background = ggplot2::element_rect(colour= 'white', fill= 'white'))

  return(plotTheme)


}


