#' Remove consecutive repeated values
#'
#'Specify maximum number of allowable consecutive identical values, or read from the 'ctrls' (which have been read from source dataset headers)
#'
#' @param rB3in rB3 object input
#' @param startDate start date
#' @param endDate endDate
#' @param varNames list of variable names or keywords
#' @param maxReps apply maximum allowed consecutive identical values filter, numerical or TRUE for from 'ctrls'
#' @param logID write an operation identifier to the log frames
#' @param Reason reason for hte logged changes (character)
#' @param showPlot display figure in plots window (TRUE/FALSE)
#' @param savePlot save figure to a path (TRUE/FALSE)
#' @keywords wrangling
#' @export
#' @examples new <- filterReps(old, varNames = 'TmpWtr', maxReps = 20, showPlot = TRUE)

filterReps <- function(rB3in, startDate, endDate, varNames, maxReps, logID, Reason, showPlot, savePlot) {

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

  if (missing(maxReps)){
    maxReps <- TRUE
  }

  if (missing(logID)){
    logID <- "Repeats"
  }

  if (missing(Reason)){
    Reason <- "Removed repeated values"
  }

  if (missing(showPlot)){
    showPlot <- FALSE
  }

  if (missing(savePlot)) {
    savePlot <- NULL
  }

  ######## end defaults ########


  ######## function ########

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
  df <- rB3in[["qcDF"]]

  # make blank df for highlighting in plots
  hlDF <- df
     hlDF[1:nrow(hlDF),2:ncol(hlDF)]   <- NA

  # col names for later use
  DFColNames <- colnames(df)


  # set filter thresholds for repeated values
  if (is.numeric(maxReps)) {
    filts <- rep(maxReps,nrow(rB3new[["ctrls"]]))
  } else {
  filts <- rB3new[["ctrls"]]$maxReps }


  #### body of function

  ##### detect rows for each col [i]
  for (i in 1:length(colLocsNums)){

       # identify repition threshold
      maxRep <- as.numeric(as.character(filts[(colLocsNums[i]- 1)]) )


  ##### algorithm to identify repeats and remove #####

  # dataframe columns to modify
  thisCol <- data.frame(rB3new[["qcDF"]] [colLocsNums[i]])

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

  #>> make changes

  ## write data that will be removed to highlighting DF
  hlDF[thisCol$rowsToChange,colLocsNums[i]] <- df[thisCol$rowsToChange,colLocsNums[i]]

  ### replace by NA in working DF
  df[thisCol$rowsToChange,colLocsNums[i]] <- NA

  ### write to same portion of logDF
  rB3new[["logDF"]] [thisCol$rowsToChange,colLocsNums[i]] <- ifelse(is.na(rB3new[["logDF"]] [thisCol$rowsToChange,colLocsNums[i]]),
                                                            logID,
                                                            paste0(rB3new[["logDF"]] [thisCol$rowsToChange,colLocsNums[i]], ' : ',logID ) )


    }



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
