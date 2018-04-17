#' shiny plot - visually assist data transformation information
#'
#' Plot single timeseries and return x and y informatoin in response to clicks. Use ggplot. Can zoom in.
#' 
#' @export
#' @param DF_in data frame input
#' @param startDate start date
#' @param endDate endDate
#' @param varNames name of the variable
#' @param colNum location of the column
#' @keywords wrangling
#' @examples newDF <- exclude_vars(myDF,metaData,varNames = c("pH","wndDir"))
#' 

shinySet <- function(DF_in, startDate, endDate, varNames, colNum){
  
  ######## log making 1 ######## 
  # check if DF is a list 
  # default: input log does not exist
  
  log_exist <- FALSE
  inLog <- NULL
  
  if (!is.data.frame(DF_in)){
    inLog <- DF_in[[2]]
    outLog <- inLog
    log_exist <- TRUE
    
    DF_in <- DF_in[[1]]
  } 
  ######## end log making 1 ######## 
  
  
  ######## defaults ########
  
  if (missing(startDate)){
    startDate <- DF_in$DateTime[1]
  }
  
  if (missing(endDate)){
    endDate <- DF_in$DateTime[length(DF_in$DateTime)]
  }
  
  if (missing(varNames)){
    varNames <- NULL
  }
  
  if (missing(colNum)){
    colNum <- NULL
  }
  
  ######## end defaults ########
  
  
  ######## function ######## 
  # identify the elements in the array
  if (is.null(varNames)){
    varNames <- colnames(DF_in[colNum])
  }
  
  outs.idElToModify <- idElToModify(DF_in, startDate = startDate, endDate = endDate, varNames = varNames)
  
  # decompose the list
  rowLocs <- outs.idElToModify[[1]]
  rowLocsNums <- which(rowLocs)
  colLocs <- outs.idElToModify[[2]]
  colLocsNums <- which(colLocs)
  
  DF_plot <- DF_in[rowLocsNums,c(1,colLocsNums)]
  ######## end function ######## 
  
  
  ######## Shiny ######## 
  
  ui <- shiny::basicPage(
    shiny::plotOutput("plot1",
                      click = "plot_click",
                      dblclick = "plot_dblclick",
                      hover = "plot_hover",
                      brush = "plot_brush"
    ),
    shiny::verbatimTextOutput("info")
  )
  
  server <- function(input, output) {
    ranges <- reactiveValues(x = NULL, y = NULL)
    
    output$plot1 <- shiny::renderPlot({
      # plot(DF_plot, type = "p")
      ggplot2::ggplot(DF_plot, ggplot2::aes(DF_plot[,1], DF_plot[,-1])) +
        ggplot2::geom_point() +
        ggplot2::coord_cartesian(xlim = ranges$x, ylim = ranges$y, expand = FALSE)
    })
    
    shiny::observeEvent(input$plot_dblclick, 
                        {
                          brush <- input$plot_brush
                          if (!is.null(brush)) {
                            ranges$x <- c(
                              as.POSIXct(round(brush$xmin - (brush$xmax-brush$xmin)*0.05, 1),
                                         origin = "1970-01-01 00:00:00",
                                         format = "%Y-%m-%d %H:%M:%S",
                                         tz = "Etc/GMT+12"),
                              as.POSIXct(round(brush$xmax + (brush$xmax-brush$xmin)*0.05, 1),
                                         origin = "1970-01-01 00:00:00",
                                         format = "%Y-%m-%d %H:%M:%S",
                                         tz = "Etc/GMT+12")
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
                          tz = "Etc/GMT+12"),
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
                          tz = "Etc/GMT+12"),
               " | ymin = ",
               round(e$ymin, 1),
               "\n",
               "     xmax = ",
               as.POSIXct(round(e$xmax, 1),
                          origin = "1970-01-01 00:00:00",
                          format = "%Y-%m-%d %H:%M:%S",
                          tz = "Etc/GMT+12"), 
               " | ymax = ",
               round(e$ymax, 1),
               "\n")
      }
      
      xy_example_1 <- function(e) {
        if(is.null(e)) return("NULL\n")
        paste0("     DF_out <- assign_na(DF_in, metaD,\n",
               "           startDate = \"",
               as.POSIXct(round(e$xmin, 1),
                          origin = "1970-01-01 00:00:00",
                          format = "%Y-%m-%d %H:%M:%S",
                          tz = "Etc/GMT+12"),
                "\",\n           endDate = \"",
               as.POSIXct(round(e$xmax, 1),
                          origin = "1970-01-01 00:00:00",
                          format = "%Y-%m-%d %H:%M:%S",
                          tz = "Etc/GMT+12"),
                "\",\n           varNames = \"",
               as.character(colnames(myDF[2])),
                "\")"
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
  
  return(list(ui,server,DF_plot))
  ######## end Shiny ######## 
}