#' Detect and remove outliers based on running mean with standard deviation
#'
#' Specify a time window, and number of SD allowed manually, or read from the 'ctrls' (which have been read from source dataset headers)
#'
#' @param rB3in rB3 object input
#' @param startDate start date
#' @param endDate endDate
#' @param varNames list of variable names or keywords
#' @param avgWindow time in hrs to calculate running mean and SD
#' @param nSD number of stdard deviations about mean which is considered an outlier
#' @param logID write an operation identifier to the log frames, default = NA
#' @param showPlot display figure in plots window (TRUE/FALSE)
#' @param savePlot save figure to a path (TRUE/FALSE)
#' @keywords wrangling
#' @export
#' @examples newDF <- exclude_vars(myDF,metaData,varNames = c("pH","wndDir"))

filterMsd <- function(rB3in, startDate, endDate, varNames, avgWindow, nSD, logID, showPlot, savePlot) {

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

  if (missing(avgWindow)){
    avgWindow <- 24
  }

  if (missing(nSD)){
    nSD <- 3
  }

  if (missing(logID)){
    logID <- "MeanSD"
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
  rB3in <- writeLog(rB3in = rB3in, logID = logID, funName = "MeanSD", Reason = "Exceeds Mean +/- n(SDs)" )
  browser()

  nrowCtrls <- as.numeric(as.character(nrow(rB3in[["ctrls"]]) ))

  # set filter thresholds for repeated values
  # if (is.numeric(avgWindow))  {

  if (is.numeric(avgWindow)){
    aa <- 3

    tester <- rep(avgWindow,nrow(rB3in[["ctrls"]]))
  } else {
    tester <- rB3in[["ctrls"]]$filterMean }

  if (3 > 2){
    aa <- 3
    } else {
    aa <- 4}


  means <- ifelse(is.numeric(avgWindow),  rep(avgWindow,nrowCtrls) , rB3in[["ctrls"]]$filterMean )
  SDs <- ifelse(is.numeric(nSD),  rep(nSD,nrowCtrls) , rB3in[["ctrls"]]$filterSD )

  means <- if (is.numeric(avgWindow))  data.frame(rep(avgWindow,nrowCtrls) )  else  2


  for (i in 1:length(colLocsNums)){
    # name of the column to be changed
    thisColName <- DFColNames[colLocsNums[i]]

    # find conditional locations in the df
    rowsToChange <- df[rowLocsNums,colLocsNums[i]]
    rowsToChange <- intersect(rowLocsNums,rowsToChange)

    # working df
    varDF <- df[ rowsToChange, c(1,colLocsNums[i]) ]

    # find time window for var (numeric in seconds)
    varWindow <- as.numeric(as.character(pers[(colLocsNums[i]- 1)]) )

    # find location of first measurement after time window has elapsed
    firstMeas <- varDF[1, "DateTime"]
    firstWindow <- varDF[varDF$DateTime > firstMeas + varWindow * 3600,"DateTime"]


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
