#' Assign NA based on filter values from the "ctrls" data frame
#'
#' This function assigns NA to those values which are repeats, beyond a specified limit of allowable consecutive identical values
#'
#' @param rB3in rB3 object input
#' @param startDate start date
#' @param endDate endDate
#' @param varNames list of variable names or keywords
#' @param maxReps apply maximum allowed consecutive identical values filter, numerical or TRUE for from 'crtls'
#' @param logID write an operation identifier to the log frames, default = NA
#' @param showPlot display figure in plots window (TRUE/FALSE)
#' @param savePlot save figure to a path (TRUE/FALSE)
#' @keywords wrangling
#' @export
#' @examples newDF <- exclude_vars(myDF,metaData,varNames = c("pH","wndDir"))

filterReps <- function(rB3in, startDate, endDate, varNames, maxReps, logID, showPlot, savePlot) {

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

  if (missing(maxReps)){
    maxReps <- TRUE
  }

  if (missing(logID)){
    logID <- "filterReps"
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

    # write to the logKey
  rB3in <- writeLog(rB3in, logID, funName = "maxReps", Reason = "Repeated identical values" )


  # set filter thresholds for repeated values
  if (is.numeric(maxReps)) {
    filts <- rep(maxReps,nrow(rB3in[["ctrls"]]))
  } else {
  filts <- rB3in[["ctrls"]]$maxReps }


  ##### detect rows for each col [i]
  for (i in 1:length(colLocsNums)){

       # identify repition threshold
      maxRep <- as.numeric(as.character(filts[(colLocsNums[i]- 1)]) )


    ##### algorithm to identify repeats and remove #####

  # dataframe columns to modify
  thisCol <- data.frame(rB3in[["qcDF"]] [colLocsNums[i]])

  # compare with the next element and if true assign same ID
  neighbourComp <- c(TRUE,thisCol[-1L,1] != thisCol[-length(thisCol[,1]),1])
  neighbourComp[is.na(thisCol[,1])] <- TRUE
  neighbourComp[is.na(neighbourComp)] <- TRUE

  neighbourComp <- with(df, cumsum(neighbourComp))

  repeatCounts <- ave(seq_along(df[,1]),
                      neighbourComp, FUN=seq_along)

  # find repeats that exceeded max repeats
  excessRepeatLocs <- which(repeatCounts >= maxRep & !is.na(df[,1]))

  # identify all sequence locations that contains too many repeats

  thisCol$rowsToChange <- FALSE
  thisCol$rowsToChange[which(neighbourComp %in% neighbourComp[excessRepeatLocs])] <- TRUE

  # outside user date range = do not modify
  thisCol$rowsToChange[-rowLocsNums] <- FALSE

  # replace by NA
  df[thisCol$rowsToChange,colLocsNums[i]] <- NA

  # rowLocs[,i] <- df$rowsToChange[]

#
#     for (i in 1:length(colLocsNums)){
#
#     # find conditional location in the df
#     rowsToChange <- which( df[,colLocsNums[i]] <= as.numeric(as.character(filts[colLocsNums[i - 1]])) )
#     rowsToChange <- intersect(rowLocsNums,rowsToChange)
#
#     # replace by new value
#     df[rowsToChange,colLocsNums[i]] <- NA

    ### write to same portion of logDF
    rB3in[["logDF"]] [thisCol$rowsToChange,colLocsNums[i]] <- logID

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
