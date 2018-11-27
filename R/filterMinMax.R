#' Remove values outside of specified minimum and maximum bounds
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
#' @param Reason reason for the logged changes (character)
#' @param showPlot display figure in plots window (TRUE/FALSE)
#' @param savePlot save figure to a path (TRUE/FALSE)
#' @keywords wrangling
#' @export
#' @examples new <- filterReps(old, varNames = 'TmpWtr', filterMin = 9, filterMax = 25,logID = 10, showPlot = TRUE)

filterMinMax <- function(rB3in, startDate, endDate, varNames, filterMin, filterMax, logID, Reason, showPlot, savePlot) {

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

  if (missing(filterMin)){
    filterMin <- FALSE
  }

  if (missing(filterMax)){
    filterMax <- FALSE
  }

  if (missing(logID)){
    logID <- "Out-of-bounds"
  }

  if (missing(Reason)){
    Reason <- "RValue oiutside min/max bounds"
  }

  if (missing(showPlot)){
    showPlot <- FALSE
  }

  if (missing(savePlot)) {
    savePlot <- NULL
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

  # make preview rB3 object
  rB3new <- rB3in

  # copy qcDF to working DF
  df <- rB3new[["qcDF"]]

  # make blank df for highlighting in plots
  hlDF <- df
     hlDF[1:nrow(hlDF),2:ncol(hlDF)]   <- NA

  # col names for later use
  DFColNames <- colnames(df)

  # write to the logKey
  rB3new <- writeLog(rB3new, paste0(logID,"_min") , funName = "filterMinMax", Reason = Reason)

  ###### filter below minimum values
  if (filterMin != FALSE) {

  # determine value of filts for the cols affected
  if (filterMin == TRUE) { filts <- rB3new[["ctrls"]]$minVal

  } else if (is.numeric(filterMin)) {

    filts <- rep(filterMin,nrow(rB3new[["ctrls"]]))

  } else { rep(-1000000000,nrow(rB3new[["ctrls"]])) }



    # find locations
    for (i in 1:length(colLocsNums)){

    # find conditional location in the df
    rowsToChange <- which( df[,colLocsNums[i]] <= as.numeric(as.character(filts[(colLocsNums[i] - 1)]) ) )
    rowsToChange <- intersect(rowLocsNums,rowsToChange)

    ## write to highlighting DF
    hlDF[rowsToChange,colLocsNums[i]] <- df[rowsToChange,colLocsNums[i]]

    # replace by new value
    df[rowsToChange,colLocsNums[i]] <- NA

    ### write to same portion of logDF
    rB3new[["logDF"]] [rowsToChange,colLocsNums[i]] <- ifelse(is.na(rB3new[["logDF"]] [rowsToChange,colLocsNums[i]]),
                                                              logID,
                                                              paste0(rB3new[["logDF"]] [rowsToChange,colLocsNums[i]], ' : ',logID,"_min" ) )


    }
  }


  ###### filter above maximum values
  if (filterMax != FALSE) {

    # determine value of filts for the cols affected
    if (filterMax == TRUE) { filts <- rB3new[["ctrls"]]$maxVal

    } else if (is.numeric(filterMax)) {

      filts <- rep(filterMax,nrow(rB3new[["ctrls"]]))

    } else { rep(-1000000000,nrow(rB3new[["ctrls"]])) }


    if (filterMax != FALSE) {
      rB3new <- writeLog(rB3new, logID, funName = "filterMinMax", Reason = Reason)
    }


    # find locations
    for (i in 1:length(colLocsNums)){

      # find conditional location in the df
      rowsToChange <- which( df[,colLocsNums[i]] >= as.numeric(as.character(filts[(colLocsNums[i] - 1)]) ) )
      rowsToChange <- intersect(rowLocsNums,rowsToChange)

      ## write to highlighting DF
      hlDF[rowsToChange,colLocsNums[i]] <- df[rowsToChange,colLocsNums[i]]

      # replace by new value
      df[rowsToChange,colLocsNums[i]] <- NA

      ### write to same portion of logDF
      rB3new[["logDF"]] [rowsToChange,colLocsNums[i]] <- ifelse(is.na(rB3new[["logDF"]] [rowsToChange,colLocsNums[i]]),
                                                               logID,
                                                               paste0(rB3new[["logDF"]] [rowsToChange,colLocsNums[i]], ' : ',logID ,"_max") )

    }
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
