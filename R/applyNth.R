#' Apply an nth order transfomration
#'
#' Apply a mathematical transformation, e.g. new = a + b(old) + c(old)^2 + c(old)^3 + ...etc
#'
#' @export
#' @param rB3in rB3 object input
#' @param startDate start date
#' @param endDate endDate
#' @param varNames list of variable names or keywords
#' @param coeffs a vector of coefficients, from 0 to nth order (e.g., c(2,3,4,5) == 5x^3 + 4x^2 + 3x + 2)
#' @param showPlot display figure in plots window (TRUE/FALSE)
#' @param savePlot save figure to a path (TRUE/FALSE)
#' @keywords transformations
#' @examples rB3in <- applyEqn(rB3in = rB3in,varNames = 'TmpWtr', coeffs = c(1,1,1,1), showPlot = TRUE)
#'
#'

applyNth <- function(rB3in,startDate,endDate,varNames, coeffs, logID, Reason, showPlot, savePlot){


  ######## set defaults ########
  if (missing(startDate)){
    startDate <- rB3in[["qcDF"]]$DateTime[1]
  }

  if (missing(endDate)){
    endDate <- rB3in[["qcDF"]]$DateTime[length(rB3in[["qcDF"]]$DateTime)]
  }

  if (missing(coeffs)){
    coeffs <- NA
  }

  if (missing(logID)){
    logID <- "transform"
  }

  if (missing(Reason)){
    Reason <- "nth order transformation"
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


  # copy qcDF to working DF
  df <- rB3in[["qcDF"]]

  # make preview rB3 object
  rB3new <- rB3in

  # make blank df for highlighting in plots
  hlDF <- df
  hlDF[1:nrow(hlDF),2:ncol(hlDF)]   <- NA


  ####

  # assign empty


  for (i in 1:length(colLocsNums)){

    oldVal <- df[rowLocsNums,colLocsNums[i]]

    df[rowLocsNums,colLocsNums[i]] <- 0

      for (z in 1:length(coeffs)){

    df[rowLocsNums,colLocsNums[i]] <- df[rowLocsNums,colLocsNums[i]] + coeffs[z] * oldVal^(z-1)


    }


  }

  #>> make changes

  ## write drift-corrected data to highlighting DF
  hlDF[rowLocsNums,colLocsNums] <- df[rowLocsNums,colLocsNums]

  ### write to same portion of logDF
  rB3new[["logDF"]] [rowLocsNums,colLocsNums[i]] <- ifelse(is.na(rB3new[["logDF"]] [rowLocsNums,colLocsNums[i]]),
                                                            logID,
                                                            paste0(rB3new[["logDF"]] [rowLocsNums,colLocsNums[i]], ' : ',logID ))

  ##### plotting #######

  rB3new[["hlDF"]] <- hlDF

  # end function and return rB3 object if no showPlot
  if (showPlot != TRUE) {
    # always return changes if no showPlot

    rB3new[["hlDF"]] <- NULL

  } else {

    # if showPlot == TRUE, generate prompt and ask to accept
    if (showPlot == TRUE | !is.null(savePlot)) {
      prePostPlot(rB3new, startDate, endDate, varNames = varNames,
                  srcColour = 'grey', hlColour = 'red', qcColour = 'blue', showPlot = showPlot, savePlot = savePlot, dpi = 200)


      if (menu(c("Yes", "No"), title="Apply these changes?") == 1){

        if (!is.null(savePlot)) {


          ggplot2::ggsave(paste0(savePlot, rB3in[["metaD"]]$siteName,"_facet.png"),
                          height = 1.2 * length(unique(plotAll$var)),
                          width = 7.5,
                          dpi = dpi)
        }

        #> write the changes

        # copy working df to source df
        rB3new[["qcDF"]] <- df

        rB3new[["hlDF"]] <- NULL
        print('Changes have been applied')

        # ..or don't
      } else {
        rB3new <- rB3in
        print ( 'Changes were not applied' )
        }

    }   # end showPlot == TRUE loop

  }   # end showPlot loop

  # return the modified rB3 object
  return(rB3new)

  ######## end function ########
}
