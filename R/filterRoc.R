#' Remove values that exceed rate of change - replace with NA
#'
#' Specify a maximum ROC manually, or read from the 'ctrls' (which have been read from source dataset headers)
#'
#' @param rB3in rB3 object input
#' @param startDate start date
#' @param endDate endDate
#' @param varNames list of variable names or keywords
#' @param maxRoc maximum allowed rate of change between consecutive values (irrespective of time diff)
#' @param logID write an operation identifier to the log frames, default = NA
#' @param showPlot display figure in plots window, and prompt to accept changes (TRUE/FALSE)
#' @param savePlot save the figure to a path (provide path relative to working directory)
#' @keywords outliers
#' @export
#' @examples newDF <- exclude_vars(myDF,metaData,varNames = c("pH","wndDir"))

filterRoc <- function(rB3in, startDate, endDate, varNames, maxRoc, logID, Reason, showPlot, savePlot) {

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
    maxRoc <- TRUE
  }

  if (missing(logID)){
    logID <- ">ROC"
  }

  if (missing(Reason)){
    Reason <- "Removed values exceeding rate of change"
  }

  if (missing(showPlot)){
    showPlot <- FALSE
  }

  if (missing(savePlot)) {
    savePlot <- NULL
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

  # make preview rB3 object
  rB3new <- rB3in

  # copy qcDF to working DF
  df <- rB3new[["qcDF"]]

  # make blank df for highlighting in plots
  hlDF <- df
  hlDF[1:nrow(hlDF),2:ncol(hlDF)]   <- NA

  # col names for later use
  DFColNames <- colnames(df)

  #### body of function

  # set filter thresholds for repeated values
  if (is.numeric(maxRoc)) {
    filts <- rep(maxRoc,nrow(rB3new[["ctrls"]]))
  } else {
    filts <- rB3new[["ctrls"]]$maxRoc }


  # write to the logKey
  rB3new <- writeLog(rB3new, logID = logID, funName = "filterROC", Reason = Reason)

  # loop through vars
   for (i in 1:length(colLocsNums)){
    # name of the column to be changed
    thisColName <- DFColNames[colLocsNums[i]]

    # find conditioned location in the df
    rowsToChange <- which(abs(diff(df[,colLocsNums[i]], lagDiff)) > as.numeric(as.character(filts[(colLocsNums[i]- 1)])))
    rowsToChange <- intersect(rowLocsNums,rowsToChange)

    ### write to logDF
    rB3new[["logDF"]] [rowsToChange,colLocsNums[i]] <- ifelse(is.na(rB3new[["logDF"]] [rowsToChange,colLocsNums[i]]),
                                                             logID,
                                                             paste0(rB3new[["logDF"]] [rowsToChange,colLocsNums[i]], ' : ',logID ) )

    ## write newVal highlighting DF
    hlDF[rowsToChange,colLocsNums[i]] <- df[rowsToChange,colLocsNums[i]]

    ### write NA to df where rate of change was exceeded
    df[rowsToChange,colLocsNums[i]] <- NA

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
