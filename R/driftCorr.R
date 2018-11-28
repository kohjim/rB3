#' Apply drift correction
#'
#' Change offset and slope over time to account for linear measurement drift
#'
#' @export
#' @param rB3in rB3 object input
#' @param startDate start date
#' @param endDate endDate
#' @param logID assign log ID
#' @param varNames variable to which drift correction will be applied
#' @param lowRef a reference/known low value at start of period
#' @param lowStart equivalent sensor value at start of period which should = lowRef  (i.e., starting offset)
#' @param lowEnd equivalent sensor value at end of period which should = lowRef  (i.e., finishing offset)
#' @param highRef a reference/known high value at start of period
#' @param highStart equivalent sensor value at start of period which should = lowRef  (i.e., starting offset)
#' @param highEnd equivalent sensor value at end of period which should = lowRef  (i.e., finishing offset)
#' @param logID write an operation identifier to the log frames, default = NA
#' @param showPlot display figure in plots window (TRUE/FALSE)
#' @param savePlot save figure to a path (TRUE/FALSE)
#' @keywords transformations
#' @examples rB3in <- driftCorr(rB3in = myDF,varNames = "DOpsat.d00050",lowRef = 0, lowStart = -10,lowEnd = 0, highRef = 100, highStart = 85, highEnd = 130, showPlot = TRUE)
#'
#'

driftCorr <- function(rB3in,startDate,endDate,varNames, lowRef, lowStart, lowEnd, highRef, highStart, highEnd, logID, Reason, showPlot, savePlot){


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

  if (missing(logID)){
    logID <- "driftCorr"
  }

  if (missing(Reason)){
    Reason <- "Corrected for measurement drift"
  }

  if (missing(showPlot)){
    showPlot <- FALSE
  }

  if (missing(savePlot)) {
    savePlot <- NULL
  }


  ######## end set defaults ########


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

  # col names for later use
  DFColNames <- colnames(df)

  ######### FUNCTION GOES HERE ###########

  # find slope and offset adjustment at start of period
  offsetStart <- lowRef - lowStart
  slopeStart  <- (highRef - lowRef ) / (highStart - lowStart)

  # find slope and offset adjustment at end of period
  offsetEnd <- lowRef - lowEnd
  slopeEnd  <- (highRef - lowRef) / (highEnd - lowEnd)

  # interpolate slope and offset
  offsets <- seq(from = offsetStart, to = offsetEnd, length.out = length(rowLocsNums))
  slopes <- seq(from = slopeStart, to = slopeEnd, length.out = length(rowLocsNums))

  for (i in 1:length(colLocsNums)){

  # apply adjustments through time
  for (l in rowLocsNums) {

    df[l,colLocsNums[i]] <- df[l,colLocsNums[i]] * slopes[l] + offsets[l]
    }

    ## write drift-corrected data to highlighting DF
    hlDF[rowLocsNums,colLocsNums[i]] <- df[rowLocsNums,colLocsNums[i]]

    ### write to same portion of logDF
    rB3new[["logDF"]] [rowLocsNums,colLocsNums[i]] <- ifelse(is.na(rB3new[["logDF"]] [rowLocsNums,colLocsNums[i]]),
                                                             logID,
                                                             paste0(rB3new[["logDF"]] [rowLocsNums,colLocsNums[i]], ' : ',logID ))

  }

  ##########################>> make changes


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
