#' Interactive data visualisation
#'
#' Plot single timeseries of source and quality controlled data. Can zoom in (dbl-click). \cr
#'   Return x and y information in response to clicks. \cr
#'   Example code is generated for removing (set to NA) the highlighted area of the plot
#'
#' @export
#' @param rB3in rB3 object to be displayed
#' @param startDate start date for plot
#' @param endDate end date for plot
#' @param varNames name of the (single!) variable to be plotted
#' @param colNum location of the column, alternative to varNames
#' @param srcColour colour of the unmodified data (leave out to plot only quality controlled data)
#' @param qcColour colour of the quality controlled data (leave out to plot only unmodified/raw data)
#' @keywords visual editing
#' @examples shinySetOut <- shinySet(newDF, varNames = "TmpWtr.d00500", endDate = '2018-07-01')
#'

shinyrB3 <- function(rB3in, startDate, endDate){

  ######## defaults ########
  if (missing(startDate)){
    startDate <- rB3in[["qcDF"]]$DateTime[1]
  }

  if (missing(endDate)){
    endDate <- rB3in[["qcDF"]]$DateTime[length(rB3in[["qcDF"]]$DateTime)]
  }

  plotLabels <- rB3in[["ctrls"]]$plotLabels

  qcColour <- 'black'
  
  srcColour <- 'red'

  isPlotSrc <- FALSE

  ######## end defaults ########


  ######## function ########

  # find name of input rB3object, to be used for example function
  rB3name <- deparse(substitute(rB3in))
  
  varNames = rownames(rB3agg2[['ctrls']])[1] # default
  
  plotAll <- shiny_mkDF(
    rB3in = rB3in, 
    startDate = startDate,
    endDate = endDate,
    varNames = varNames,
    isPlotSrc = isPlotSrc)
  
  srcGeom <- ggplot2::geom_point(
    ggplot2::aes(
      x = DateTime,
      y = src,
      color = "Unmodified data"
      ),
    size = 0.2
    )
  
  qcGeom <- ggplot2::geom_point(
    ggplot2::aes(
      x = DateTime,
      y = qc,
      color = "Quality controlled data"
    ),
    size = 0.2
  )
  
  srcKey <- ggplot2::scale_colour_manual(
    "",
    values = c("Unmodified data"=srcColour)
  )
  qcKey <- ggplot2::scale_colour_manual(
    "",
    values = c("Quality controlled data"=qcColour)
  )
  
  dualKey <- ggplot2::scale_colour_manual(
    "",
    values = c(
      "Unmodified data"=srcColour,
      "Quality controlled data"=qcColour
    )
  )
  
  
  ######## end AES #########
  
  ######## Shiny ########
  
  ui <- shiny::fluidPage(
    shiny::fluidRow(
      
    ),
    
    shiny::fluidRow(
      shiny::column(
        4,
        selectInput(
          "varNames",
          NULL,
          rownames(rB3agg2[['ctrls']])
        )
      ),
      
      shiny::column(
        4,
        checkboxInput(
          "isPlotSrc",
          "Plot src",
          value = FALSE
        )
      )
    ),
    
    shiny::fluidRow(
      shiny::column(
        12,
        shiny::plotOutput("plot1",
                          click = "plot_click",
                          dblclick = "plot_dblclick",
                          hover = "plot_hover",
                          brush = "plot_brush"
        )
      )
    ),
    
    shiny::verbatimTextOutput("info")
  )

  server <- function(input, output) {
    ranges <- reactiveValues(x = NULL, y = NULL)

    output$plot1 <- shiny::renderPlot({
      
      plotAll <- shiny_mkDF(
        rB3in = rB3in,
        startDate = startDate,
        endDate = endDate,
        varNames = input$varNames,
        isPlotSrc = input$isPlotSrc)
      
      varPlot <-
        ggplot2::ggplot(plotAll) +
        ggplot2::ylab("Value") +
        ggplot2::xlab(NULL) +
        ggplot2::scale_x_datetime(
          labels = scales::date_format("%Y-%m"),
          breaks = scales::date_breaks("1 years"),
          limits = c(min(plotAll$DateTime),max(plotAll$DateTime)),
          expand = c(0, 0)
        ) +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 0, hjust = 0)) +
        ggplot2::facet_wrap(
          ~var, 
          ncol = 1,
          scales = 'free_y'
        ) +
        ggplot2::theme_bw() +
        ggplot2::theme(legend.position = "bottom")
      
      if (!input$isPlotSrc) {
        varPlot +
          qcGeom +
          qcKey +
          ggplot2::coord_cartesian(xlim = ranges$x, ylim = ranges$y, expand = FALSE)
        
      } else {
        varPlot +
          srcGeom +
          qcGeom +
          dualKey +
          ggplot2::coord_cartesian(xlim = ranges$x, ylim = ranges$y, expand = FALSE)
      }
      
    })
    
    shiny::observeEvent(
      input$plot_dblclick,
      {
        brush <- input$plot_brush
        if (!is.null(brush)) {
          ranges$x <- c(
            as.POSIXct(round(brush$xmin - (brush$xmax-brush$xmin)*0.05, 1),
                       origin = "1970-01-01 00:00:00",
                       format = "%Y-%m-%d %H:%M:%S",
                       tz = "UTC"),
            as.POSIXct(round(brush$xmax + (brush$xmax-brush$xmin)*0.05, 1),
                       origin = "1970-01-01 00:00:00",
                       format = "%Y-%m-%d %H:%M:%S",
                       tz = "UTC")
          )
          ranges$y <- c(brush$ymin - (brush$ymax - brush$ymin)*0.05,
                        brush$ymax + (brush$ymax - brush$ymin)*0.05)
          
        } else {
          ranges$x <- NULL
          ranges$y <- NULL
        }
      }
    )

    output$info <- shiny::renderText({
      xy_str <- function(e) {
        if(is.null(e)) return("NULL\n")
        paste0("x = ",
               as.POSIXct(round(e$x, 1),
                          origin = "1970-01-01 00:00:00",
                          format = "%Y-%m-%d %H:%M:%S",
                          tz = "UTC"),
               " | y = ",
               round(e$y, 1),
               "\n")
      }

      xy_range_str <- function(e) {
        if(is.null(e)) return("NULL\n")
        paste0("     xmin = ",
               as.POSIXct(round(e$xmin, 1),
                          origin = "1970-01-01 00:00:00",
                          format = "%Y-%m-%d %H:%M:%S",
                          tz = "UTC"),
               " | ymin = ",
               round(e$ymin, 1),
               "\n",
               "     xmax = ",
               as.POSIXct(round(e$xmax, 1),
                          origin = "1970-01-01 00:00:00",
                          format = "%Y-%m-%d %H:%M:%S",
                          tz = "UTC"),
               " | ymax = ",
               round(e$ymax, 1),
               "\n")
      }

      xy_example_1 <- function(e) {
        if(is.null(e)) return("NULL\n")
        paste0(rB3name, " <- assignVal(", rB3name, ", varNames = \"",
               as.character(input$varNames),
               "\",  \n          startDate = \"",
               as.POSIXct(round(e$xmin, 1),
                          origin = "1970-01-01 00:00:00",
                          format = "%Y-%m-%d %H:%M:%S"),
               "\", endDate = \"",
               as.POSIXct(round(e$xmax, 1),
                          origin = "1970-01-01 00:00:00",
                          format = "%Y-%m-%d %H:%M:%S"),
               "\", \n          minVal = ",
               round(e$ymin, 1),
               ", maxVal = ",
               round(e$ymax, 1),
               ', newVal = NA, logID = "Shiny", Reason = "Manual removal") #, showPlot = T)'
        )
        }

      paste0(
        "Click: ", xy_str(input$plot_click),
        "Double Click: ", xy_str(input$plot_dblclick),
        "Rectangle: \n", xy_range_str(input$plot_brush),
        "Example 1: \n", xy_example_1(input$plot_brush)
      )
    })
  }

  # shiny::shinyApp(ui = ui, server = server)

  shinySetOut <- (list(ui,server,plotAll))

  shiny::shinyApp(ui = shinySetOut[[1]], server = shinySetOut[[2]])

  ######## end Shiny ########
}
