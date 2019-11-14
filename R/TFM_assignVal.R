#' Assign a value or NA to the specified dates range and variables
#'
#' Specify subset of dates, values and variables to modify
#'
#' @param rB3in rB3 object input
#' @param startDate start date
#' @param endDate endDate
#' @param varNames list of variable names or keywords
#' @param minVal delete value greater than or equal to
#' @param maxVal delete value less than or equal to
#' @param logID write an operation identifier to the log frames, default = NA, 'auto' chooses a log number for you, or provide a numerical code
#' @param showPlot display figure in plots window, and prompt to accept changes (TRUE/FALSE)
#' @param savePlot save the figure to a path (provide path relative to working directory)
#' @keywords wrangling
#' @export
#' @examples newDF <- exclude_vars(myDF,metaData,varNames = c("pH","wndDir"))

assignVal <- function(rB3in, startDate, endDate, varNames, minVal, maxVal, newVal, logID, Reason, showPlot, savePlot) {

  ######## defaults #########
  if (missing(startDate)){
    startDate <- rB3in[["qcDF"]]$DateTime[1]
  }

  if (missing(endDate)){
    endDate <- rB3in[["qcDF"]]$DateTime[length(rB3in[["qcDF"]]$DateTime)]
  }

  if (missing(varNames)){
    varNames <- "All"
  }

  if (missing(minVal)){
    minVal <- -100000000000
  }

  if (missing(maxVal)){
    maxVal <- 100000000000
  }

  if (missing(newVal)){
    newVal <- NA
  }

  if (missing(logID)){
    logID <- "assignVal"
  }

  if (missing(Reason)){
    Reason <- "Value replaced"
  }

  if (missing(showPlot)){
    showPlot <- FALSE
  }

  if (missing(savePlot)) {
    savePlot <- NULL
  }

  ######## end defaults ########


  ######## function ########

########### assign_na to qcDF based on input args


  # identify the elements in the array, to be modified
  outs.idElToModify <- idElToModify(rB3in, startDate = startDate, endDate = endDate, varNames = varNames)

  # decompose the list
  rowLocs <- outs.idElToModify[[1]]
  rowLocsNums <- which(rowLocs)
  colLocs <- outs.idElToModify[[2]]
  colLocsNums <- which(colLocs)

  # make preview rB3 object
  rB3new <- rB3in

  # copy qcDF to working DF
  df <- rB3new[["qcDF"]]

  # make blank df for highlighting in plots
  hlDF <- df
     hlDF[1:nrow(hlDF),2:ncol(hlDF)]   <- NA

 #### body of function

  for (i in 1:length(colLocsNums)){

    # name of the column to be changed
    rowsToChange <- which(df[,colLocsNums[i]] >= as.numeric(as.character(minVal)) &
                            df[,colLocsNums[i]] <= as.numeric(as.character(maxVal)) |
                            is.na(df[,colLocsNums[i]])
                            )

    rowsToChange <- intersect(rowLocsNums,rowsToChange)

 if (is.numeric(newVal)) {

       # write to the logKey
      rB3new <- writeLog(rB3new, logID = logID, funName = "assign_value", Reason = Reason)

      ### write to logDF
      rB3new[["logDF"]] [rowsToChange,colLocsNums[i]] <- ifelse(is.na(rB3new[["logDF"]] [rowsToChange,colLocsNums[i]]),
                                                                logID,
                                                                paste0(rB3new[["logDF"]] [rowsToChange,colLocsNums[i]], ' : ',logID ))
      ## write newVal to highlighting DF
      hlDF[rowsToChange,colLocsNums[i]] <- newVal

      ## write newVal to highlighting DF
      df[rowsToChange,colLocsNums[i]] <- newVal

 } else {

      # write to the logKey
      rB3new <- writeLog(rB3new, logID = logID,  funName = "assign_NA", Reason = Reason)

      ### write to logDF
      rB3new[["logDF"]] [rowsToChange,colLocsNums[i]] <- ifelse(is.na(rB3new[["logDF"]] [rowsToChange,colLocsNums[i]]),
                                                                logID,
                                                                paste0(rB3new[["logDF"]] [rowsToChange,colLocsNums[i]], ' : ',logID ))


      ## write data to highlighting DF
      hlDF[rowsToChange,colLocsNums[i]] <- df[rowsToChange,colLocsNums[i]]

      ## write newVal to highlighting DF
      df[rowsToChange,colLocsNums[i]] <- newVal


      }


    }

  #>> make changes

  # copy working df to source df
  rB3new[["qcDF"]] <- df

  ##### plotting #######

  # copy current df for plotting
  rB3plot <- rB3new
  rB3plot[["hlDF"]] <- hlDF

  # apply changes to new DF
  rB3new[["qcDF"]] <- df

  # if user wants a plot of the action, generate plot and prompt to accept
  if (showPlot == TRUE | !is.null(savePlot)) {
    prePostPlot(rB3plot, startDate, endDate, varNames = varNames,
                srcColour = 'grey', hlColour = 'red', qcColour = 'blue', showPlot = showPlot, savePlot = savePlot, dpi = 200)

    if (!is.null(savePlot)) {

      ggplot2::ggsave(paste0(savePlot, rB3in[["metaD"]]$siteName,"_facet.png"),
                      height = 0.5 + 1.1 * length(unique(plotAll$var)),
                      width = 7.5,
                      dpi = dpi)
    }

    if (menu(c("Yes", "No"), title="Apply these changes?") == 1){

      print('Changes have been applied')

      # ..or don't
    } else {
      rB3new <- rB3in
      print ( 'Changes were not applied' )
    }

  }   # end plotting loop

  # return the modified rB3 object
  return(rB3new)

  ######## end function ########
}
