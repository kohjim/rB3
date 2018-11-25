#' Remove values that exceed rate of change - replace with NA
#'
#' Specify a maximum ROC manually, or read from the 'ctrls' (which have been read from source dataset headers)
#'
#' @param rB3in rB3 object input
#' @param startDate start date
#' @param endDate endDate
#' @param varNames list of variable names or keywords
#' @param maxRoc apply maximum allowed consecutive identical values filter, numerical or TRUE for from 'crtls'
#' @param logID write an operation identifier to the log frames, default = NA
#' @param showPlot display figure in plots window (TRUE/FALSE)
#' @param savePlot save figure to a path (TRUE/FALSE)
#' @keywords wrangling
#' @export
#' @examples newDF <- exclude_vars(myDF,metaData,varNames = c("pH","wndDir"))

filterRoc <- function(rB3in, startDate, endDate, varNames, maxRoc, logID, showPlot, savePlot) {

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

  if (missing(maxRoc)){
    maxRoc <- TRUE
  }

  if (missing(logID)){
    logID <- "R.O.C"
  }

  ######## end defaults ########


  ######## function ########
lagDiff <- 1

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

  # write to the logKey
  rB3in <- writeLog(rB3in = rB3in, logID = logID, funName = "filterRoc", Reason = "Exceeds max rate of change" )

  # set filter thresholds for repeated values
  if (is.numeric(maxRoc)) {
    filts <- rep(maxRoc,nrow(rB3in[["ctrls"]]))
  } else {
    filts <- rB3in[["ctrls"]]$maxRoc }


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


    if (menu(c("Yes", "No"), title="Apply these changes?") == 1){

      if (!is.null(savePlot)) {

        ggplot2::ggsave(paste0(savePlot, rB3in[["metaD"]]$siteName,"_facet.png"),
                        height = 1.2 * length(unique(plotAll$var)),
                        width = 7.5,
                        dpi = dpi)
      }


      rB3in[["qcDF"]] <- df

      return(rB3in)

    } else {}




  }   # close showPlot loop


  return(rB3in)



  ######## end function ########


}
