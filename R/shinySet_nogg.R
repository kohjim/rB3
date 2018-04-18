#' shiny plot - visually assist data transformation process
#'
#' Plot single timeseries and return x and y informatoin in response to clicks. No ggplot. No zooming.
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

shinySet_nogg <- function(DF_in, startDate, endDate, varNames, colNum){
  
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
    output$plot1 <- shiny::renderPlot({
      plot(DF_plot, type = "l") 
    })
    
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
               round(e$ymax, 1))
      }
      
      paste0(
        "Click: ", xy_str(input$plot_click),
        "Double Click: ", xy_str(input$plot_dblclick),
        "Rectangle: \n", xy_range_str(input$plot_brush)
      )
    })
  }

  # shiny::shinyApp(ui = ui, server = server)
  
  return(list(ui,server,DF_plot))
  ######## end Shiny ######## 
}