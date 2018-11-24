#' Assign NA based on filter values from the "ctrls" data frame
#'
#' Assigns NA to the selected subset of dates and variables
#'
#' @param rB3in rB3 object input
#' @param startDate start date
#' @param endDate endDate
#' @param varNames list of variable names or keywords
#' @param filterMin apply min value filter, either numeric (filterMin = <value>), or from 'crtls' (filterMin = TRUE)
#' @param filterMax apply max value filter, either numeric (filterMax = <value>), or from 'crtls' (filterMax = TRUE)
#' @param logID write an operation identifier to the log frames, default = NA
#' @param showPlot display figure in plots window (TRUE/FALSE)
#' @param savePlot save figure to a path (TRUE/FALSE)
#' @keywords wrangling
#' @export
#' @examples newDF <- exclude_vars(myDF,metaData,varNames = c("pH","wndDir"))

filterMinMax <- function(rB3in, startDate, endDate, varNames, filterMin, filterMax, logID, showPlot, savePlot) {

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

  if (missing(savePlot)) {
    savePlot <- NULL
  }

  if (missing(filterMin)){
    filterMin <- FALSE
  }

  if (missing(filterMax)){
    filterMax <- FALSE
  }

  if (missing(logID)){
    logID <- "filter"
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

  ###### filter below minimum values

  if (filterMin != FALSE) {


  # determine value of filts for the cols affected
  if (filterMin == TRUE) { filts <- rB3in[["ctrls"]]$minVal

  } else if (is.numeric(filterMin)) {

    filts <- rep(filterMin,nrow(rB3in[["ctrls"]]))

  } else { rep(-1000000000,nrow(rB3in[["ctrls"]])) }


    # write to the logKey

    if (filterMin != FALSE) {
    rB3in <- writeLog(rB3in, paste0(logID,"_min") , funName = "filterMin", Reason = "Value < min allowed")
    }

    # find locations
    for (i in 1:length(colLocsNums)){

    # find conditional location in the df
    rowsToChange <- which( df[,colLocsNums[i]] <= as.numeric(as.character(filts[(colLocsNums[i] - 1)]) ) )
    rowsToChange <- intersect(rowLocsNums,rowsToChange)

    # replace by new value
    df[rowsToChange,colLocsNums[i]] <- NA

    ### write to same portion of logDF
    rB3in[["logDF"]] [rowsToChange,colLocsNums[i]] <- paste0(logID,'_min')

    }
  }


  ###### filter above maximum values

  if (filterMax != FALSE) {


    # determine value of filts for the cols affected
    if (filterMax == TRUE) { filts <- rB3in[["ctrls"]]$maxVal

    } else if (is.numeric(filterMax)) {

      filts <- rep(filterMax,nrow(rB3in[["ctrls"]]))

    } else { rep(-1000000000,nrow(rB3in[["ctrls"]])) }


    if (filterMax != FALSE) {
      rB3in <- writeLog(rB3in, paste0(logID,"_max"), funName = "filterMax", Reason = "Value > max allowed")
    }


    # find locations
    for (i in 1:length(colLocsNums)){

      # find conditional location in the df
      rowsToChange <- which( df[,colLocsNums[i]] >= as.numeric(as.character(filts[(colLocsNums[i] - 1)]) ) )
      rowsToChange <- intersect(rowLocsNums,rowsToChange)

      # replace by new value
      df[rowsToChange,colLocsNums[i]] <- NA

      ### write to same portion of logDF
      rB3in[["logDF"]] [rowsToChange,colLocsNums[i]] <- paste0(logID,'_max')

    }
  }

  ##### plotting #######

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
