#' Interpolate the date range given for the variables specified
#'
#' Linearly interpolates gaps (NAs) within the selected subset of dates and variables
#'
#' @param rB3in rB3 object input
#' @param startDate start date
#' @param endDate endDate
#' @param varNames list of variable names or keywords
#' @param logID write an operation identifier to the log frames, default = NA, 'auto' chooses a log number for you, or provide a numerical code
#' @param showPlot display figure in plots window (TRUE/FALSE)
#' @param savePlot save figure to a path (TRUE/FALSE)
#' @keywords wrangling
#' @export
#' @examples newDF <- exclude_vars(myDF,metaData,varNames = c("pH","wndDir"))

assignInterp <- function(rB3in, startDate, endDate, varNames, logID, Reason, showPlot, savePlot) {

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
    Reason <- "Interpolation of missing data"
  }

  if (missing(showPlot)) {
    showPlot <- FALSE
  }

   if (missing(savePlot)) {
    savePlot <- NULL
  }

  if (missing(logID)){
    logID <- "Interp"
  }

  ######## end defaults ########


  # write to the logKey
  rB3in <- writeLog(rB3in = rB3in, logID = logID, funName = "assign_interp", Reason = "Interpolation of missing data" )

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

    logEntries <- rB3in[["logDF"]] [rowLocsNums,colLocsNums[i]]
    logEntries <- ifelse(is.na(df[rowLocsNums,colLocsNums[i]]), logID, logEntries)

    df[rowLocsNums,colLocsNums[i]] <- forecast::na.interp ( df[rowLocsNums,colLocsNums[i]] )

    ### write to same portion of logDF
    rB3in[["logDF"]] [rowLocsNums,colLocsNums[i]] <- logEntries

    }

  ## plotting

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
