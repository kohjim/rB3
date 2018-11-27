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

applyInterp <- function(rB3in, startDate, endDate, varNames, logID, Reason, showPlot, savePlot) {

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

  for (i in 1:length(colLocsNums)){

    # find the periods that will be interped to hlDF
    rowsToChange <- which( is.na(df[,colLocsNums[i]]) )
    rowsToChange <- intersect(rowLocsNums,rowsToChange)

    # interpolate the working DF
    df[rowLocsNums,colLocsNums[i]] <- forecast::na.interp ( df[rowLocsNums,colLocsNums[i]] )

    # write the hlDF with interpolated values
    hlDF[rowsToChange,colLocsNums[i]] <- df[rowsToChange,colLocsNums[i]]

    ### write to same portion of logDF
    rB3new[["logDF"]] [rowsToChange,colLocsNums[i]] <- ifelse(is.na(rB3new[["logDF"]] [rowsToChange,colLocsNums[i]]),
                                                              logID,
                                                              paste0(rB3new[["logDF"]] [rowsToChange,colLocsNums[i]], ' : ',logID ) )
    }



  ##### plotting #######

  rB3new[["hlDF"]] <- hlDF

  # if user wants a plot of the action, generate plot and prompt to accept
  if (showPlot == TRUE | !is.null(savePlot)) {
    prePostPlot(rB3new, startDate, endDate, varNames = varNames,
                srcColour = 'grey', hlColour = 'red', qcColour = 'blue', showPlot = showPlot, savePlot = savePlot, dpi = 200)

    if (!is.null(savePlot)) {

      ggplot2::ggsave(paste0(savePlot, rB3in[["metaD"]]$siteName,"_facet.png"),
                      height = 0.5 + 1.1 * length(unique(plotAll$var)),
                      width = 7.5,
                      dpi = dpi)
    }

    if (menu(c("Yes", "No"), title="Apply these changes?") == 1){

      # write the changes
      # copy working df to source df
      rB3new[["qcDF"]] <- df
      rB3new[["hlDF"]] <- NULL
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
