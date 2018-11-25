#' applyEqn
#'
#' This custom function applies a quadratic transformation to a specifed data range
#' of the format newVal = oldVal*coeffA^2 + oldVal*coeffB + coeffC
#'
#' @export
#' @param rB3in rB3 object input
#' @param startDate start date
#' @param endDate endDate
#' @param coeffs a vector of coefficients, from 0 to nth order (e.g., c(2,3,4,5) == 5x^3 + 4x^2 + 3x + 2)
#' @param plotPath plot figure of before and after
#' @keywords customFun
#' @examples newDF <- DOsat2DOmg_ZebraTechDOpto(DF_in = myDF,DOmgColName = "DOconc_d00050", DOsatColName = "DOpsat_d00050", TColName = "TmpDOs_d00050")
#'
#'

applyEqn <- function(DF_in,startDate,endDate,varNames, coeffs, logID, showPlot, savePlot){


  ######## set defaults ########
  if (missing(startDate)){
    startDate <- DF_in$DateTime[1]
  }

  if (missing(endDate)){
    endDate <- DF_in$DateTime[length(DF_in$DateTime)]
  }

  if (missing(coeffs)){
    coeffs <- NA
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


  ####

  for (i in 1:length(colLocsNums)){


  for (z in 1:length(coeffs)){
    df[rowLocsNums,colLocsNums[i]]  <- df[rowLocsNums,colLocsNums[i]] + coeffs[z] * df[rowLocsNums,colLocsNums[i]] ^(z-1)
  }

}

  #>> make changes

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
      rB3in <- writeLog(rB3in, logID, funName = "maxReps", Reason = "Repeated identical values" )

      # return the modified rB3 object
      return(rB3in)

      # ..or don't
    } else {}

  }   # end showPlot loop

  # always return changes if no showPlot
  return(rB3in)

  ######## end function ########
}
