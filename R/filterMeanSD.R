#' Remove values that exceed rate of change
#'
#' This function assign NA to the data larger than the assigned maxRoc
#'
#' @export
#' @param rB3in rB3 object input
#' @param startDate start date
#' @param endDate endDate
#' @param varNames list of variable names or keywords
#' @param maxRoc maximum rate of change value permitted
#' @param logID assign log ID
#' @param showPlot plot figure of before and after
#' @param savePlot plot figure of before and after
#' @keywords wrangling
#' @examples newDF <- exclude_vars(myDF,metaData,varNames = c("pH","wndDir"))
#'
#'

filterMeanSD <-  function(rB3in, maxRoc, varNames, startDate, endDate, logID, showPlot, savePlot) {


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

  if (missing(maxRoc)){
    maxRoc <- 100000000
  }

  if (missing(showPlot)){
    plotPath <- FALSE
  }

  if (missing(savePlot)){
    plotPath <- NULL
  }

  ######## end defaults ########

  ######## function ########
browser()
  df <- rB3in[["qcDF"]]

  # list of variable names
  # varList <- c("maxRoc")
  lagDiff = 1

  # col names for later use
  DFColNames <- colnames(df[,])

  # write to the logKey
  writeLog(rB3in, logID, funName = "filterRoc", Reason = "Exceeds max rate of change" )

  # set filter thresholds for repeated values
  if (is.numeric(maxRoc)) {
    filts <- rep(maxRoc,nrow(rB3in[["ctrls"]]))
  } else {
    filts <- rB3in[["ctrls"]]$filterRoc }



  # identify the elements in the array
  outs.idElToModify <- idElToModify(df, startDate = startDate, endDate = endDate, varNames = varNames)

  # decompose the list
  rowLocs <- outs.idElToModify[[1]]
  rowLocsNums <- which(rowLocs)
  colLocs <- outs.idElToModify[[2]]
  colLocsNums <- which(colLocs)


  for (i in 1:length(colLocsNums)){
    # name of the column to be changed
    thisColName <- DFColNames[colLocsNums[i]]

     # find conditioned location in the df
      rowsToChange <- which(abs(diff(df[,colLocsNums[i]], lagDiff)) > as.numeric(as.character(filts[(colLocsNums[i]- 1)])))
      rowsToChange <- intersect(rowLocsNums,rowsToChange)

      # replace by NA
      df[rowsToChange,colLocsNums[i]] <- NA

      rB3in[["logDF"]] [rowsToChange,colLocsNums[i]] <- logID

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
