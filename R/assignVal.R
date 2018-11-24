#' Assign NA to the specified dates range and variables
#'
#' This function assigns NA to the selected subset of dates and variables
#'
#' @param rB3in rB3 object input
#' @param startDate start date
#' @param endDate endDate
#' @param varNames list of variable names or keywords
#' @param minVal delete value greater than or equal to
#' @param maxVal delete value less than or equal to
#' @param logID write an operation identifier to the log frames, default = NA, 'auto' chooses a log number for you, or provide a numerical code
#' @param showPlot display figure in plots window (TRUE/FALSE)
#' @param savePlot save figure to a path (TRUE/FALSE)
#' @keywords wrangling
#' @export
#' @examples newDF <- exclude_vars(myDF,metaData,varNames = c("pH","wndDir"))

assignVal <- function(rB3in, startDate, endDate, varNames, minVal, maxVal, newVal, logID, Reason, showPlot, savePlot) {

  ######## defaults ########
  if (missing(startDate)){
    startDate <- rB3in[["qcDF"]]$DateTime[1]
  }

  if (missing(endDate)){
    endDate <- rB3in[["qcDF"]]$DateTime[length(rB3in[["qcDF"]]$DateTime)]
  }

  if (missing(varNames)){
    varNames <- "All"
  }

  if (missing(showPlot)){
    showPlot <- FALSE
  }

  if (missing(Reason)){
    Reason <- "Not specified"
  }

  if (missing(savePlot)) {
    savePlot <- NULL
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

 # apply locations to qcDF
  df <- rB3in[["qcDF"]]

  # col names for later use
  DFColNames <- colnames(df)

  for (i in 1:length(colLocsNums)){

    # name of the column to be changed
    # thisColName <- DFColNames[colLocsNums[i]]

    # find conditional location in the df
    rowsToChange <- which(df[,colLocsNums[i]] >= as.numeric(as.character(minVal)) &
                            df[,colLocsNums[i]] <= as.numeric(as.character(maxVal)) )

    rowsToChange <- intersect(rowLocsNums,rowsToChange)


 if (is.numeric(newVal)) {

      # write to the logKey
      rB3in <- writeLog(rB3in, logID = logID, funName = "assign_value", Reason = Reason)

      df[rowLocsNums,colLocsNums[i]] <- newVal

    } else {
      # write to the logKey
      rB3in <- writeLog(rB3in, logID = logID,  funName = "assign_NA", Reason = Reason)

      df[rowLocsNums,colLocsNums[i]] <- NA
      }


    ### write to same portion of logDF
    rB3in[["logDF"]] [rowsToChange,colLocsNums[i]] <- logID

    }


   rB3plot <- rB3in

   rB3plot[["preDF"]] <- rB3plot[["qcDF"]]
   rB3plot[["qcDF"]] <- df

  # generate plot, if specified

    if (showPlot == TRUE | !is.null(savePlot)) {
      prePostPlot(rB3plot, startDate, endDate, varNames = varNames,
                  srcColour = 'grey', preColour = 'red', qcColour = 'blue', showPlot = showPlot, savePlot = savePlot, dpi = 200)
    }

  rB3in[["qcDF"]] <- df



    ######## end function ########

    return(rB3in)
  }
