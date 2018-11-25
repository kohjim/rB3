#' Apply drift correction
#'
#' change slope and offset over time
#'
#' @export
#' @param rB3in rB3 object input
#' @param startDate start date
#' @param endDate endDate
#' @param logID assign log ID
#' @param plotPath plot figure of before and after
#' @param varNames variable to which drift correction will be applied
#' @param lowRef a reference/known low value at start of period
#' @param lowStart equivalent sensor value at start of period which should = lowRef  (i.e., starting offset)
#' @param lowEnd equivalent sensor value at end of period which should = lowRef  (i.e., finishing offset)
#' @param highRef a reference/known low value at start of period
#' @param highStart equivalent sensor value at start of period which should = lowRef  (i.e., starting offset)
#' @param highEnd equivalent sensor value at end of period which should = lowRef  (i.e., finishing offset)
#' @param logID write an operation identifier to the log frames, default = NA
#' @param showPlot display figure in plots window (TRUE/FALSE)
#' @param savePlot save figure to a path (TRUE/FALSE)
#' @keywords transformations
#' @examples rB3in <- driftCorr(rB3in = myDF,DOmgColName = "DOconc_d00050", DOsatColName = "DOpsat_d00050", TColName = "TmpDOs_d00050")
#'
#'

driftCorr <- function(rB3in,startDate,endDate,varNames, lowRef, lowStart, lowEnd, highRef, highStart, highEnd, logID, showPlot, savePlot){


  ######## set defaults ########
  if (missing(startDate)){
    startDate <- rB3in[["qcDF"]]$DateTime[1]
  }

  if (missing(endDate)){
    endDate <- rB3in[["qcDF"]]$DateTime[length(rB3in[["qcDF"]]$DateTime)]
  }

  if (missing(lowRef)){
    lowRef <- lowStart
  }

  if (missing(highRef)){
    highRef <- highStart
  }

  if (missing(showPlot)){
    showPlot <- FALSE
  }

  if (missing(savePlot)) {
    savePlot <- NULL
  }

  if (missing(logID)){
    logID <- "driftCorr"
  }


  ######## end set defaults ########


  # identify the elements in the array, to be modified
  outs.idElToModify <- idElToModify(rB3in, startDate = startDate, endDate = endDate, varNames = varNames)

  # decompose the list
  rowLocs <- outs.idElToModify[[1]]
    rowLocsNums <- which(rowLocs)
  colLocs <- outs.idElToModify[[2]]
    colLocsNums <- which(colLocs)

  # copy qcDF to working DF
  df <- rB3in[["qcDF"]]

  # make blank df for highlighting in plots
  hlDF <- df
  hlDF[1:nrow(hlDF),2:ncol(hlDF)]   <- NA


  ######### FUNCTION GOES HERE ###########

  # find slope and offset adjustment at start of period
  offsetStart <- lowStart - lowRef
  slopeStart  <- 1 / ( (highStart - lowStart) / (highRef - lowRef) ) - 1


  # find slope and offset adjustment at end of period
  offsetEnd <- lowEnd - lowStart
  slopeDiff  <- 1 / ( (highEnd - lowEnd) / (highStart - lowStart) ) - 1


  for (i in 1:length(colLocsNums)){


  # apply adjustments
  for (l in 1:length(rowLocsNums)) {

    df[l,colLocsNums] <- df[l,colLocsNums] *  (1 + slopeStart + (l / length(rowLocsNums) ) * slopeDiff ) - (l / length(rowLocsNums) ) * offsetEnd - offsetStart

    }

  }



  ##########################>> make changes

  ## write drift-corrected data to highlighting DF
  hlDF[rowLocsNums,colLocsNums] <- df[rowLocsNums,colLocsNums]

  ### write to same portion of logDF
  rB3in[["logDF"]] [rowLocsNums,colLocsNums] <- logID


# copy working df to source df
rB3in[["qcDF"]] <- df



   ##### plotting #######

rB3plot <- rB3in
rB3plot[["hlDF"]] <- hlDF

# if showPlot == TRUE, generate prompt and ask to accept
if (showPlot == TRUE | !is.null(savePlot)) {
  prePostPlot(rB3plot, startDate, endDate, varNames = varNames,
              srcColour = 'grey', hlColour = 'red', qcColour = 'blue', showPlot = showPlot, savePlot = savePlot, dpi = 200)


  if (menu(c("Yes", "No"), title="Apply these changes?") == 1){

    if (!is.null(savePlot)) {


      ggplot2::ggsave(paste0(savePlot, rB3in[["metaD"]]$siteName,"_facet.png"),
                      height = 1.2 * length(unique(plotAll$var)),
                      width = 7.5,
                      dpi = dpi)
    }

    # write the changes
    rB3in[["qcDF"]] <- df

    # write to the logKey
    rB3in <- writeLog(rB3in, logID, funName = "driftCorr", Reason = "Corrected for measurement drift" )

    # return the modified rB3 object
    return(rB3in)

    # ..or don't
  } else {}

}   # end showPlot loop

# always return changes if no showPlot
return(rB3in)

######## end function ########
}
