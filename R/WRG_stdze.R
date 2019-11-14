#' Buoy observation to data frame
#'
#' Trim dates and/or aggregate (to a single common timestep) an rB3 working dataframe.
#'     Use 'ctrls' to define how aggregation should be performed for each time-series(column).
#'
#' @export
#' @param rB3in rB3 object to be 'standardized'
#' @param startDate measurements prior to this time will be removed
#' @param endDate measurements after this time will be removed
#' @param varNames variables (columns) to be retained, select by full header name, key characters, or 'All'
#' @param timestep standardise (aggregate) to a common timestep (numeric, in seconds, to turn aggregation on) !! NOTE! logs will be reset to NA!!
#' @param aggAll TRUE: aggregate source and qc data separately, FALSE: source data = qc data.
#' @param methodAgg aggregation method; mean, median, sum, min, max, or circular (for averaging direction measurements in degrees)
#' @param pullAgg aggregate data from before/on new timestamp ('left'), either side of timestamp ('centre'), or on/after timestamp ('right')
#' @keywords fileIO
#' @examples stdDF <- rB3stdze(rB3in = rawDF, varNames = wqVars, metaD = metaD, startDate = '2016-07-01 00:00:00', endDate = '2018-06-30 23:45:00', doAgg = TRUE, methodAgg = ctrls$methodAgg, pullAgg = ctrls$pullAgg)
#'
#'


rB3stdze <- function(rB3in,startDate,endDate, varNames, timestep, aggAll, methodAgg, pullAgg){

  ######## defaults ########
  if (missing(startDate)){
    startDate <- min(rB3in$DateTime)
  }

  if (missing(endDate)){
    endDate <- max(rB3in$DateTime)
  }

  if (missing(timestep)){
    timestep <- NULL
    doAgg <- FALSE
  } else {
    doAgg <- TRUE
    timestep <- timestep
  }

  if (missing(aggAll)){
    aggAll <- FALSE
  }

  if (missing(methodAgg)){
    methodAgg <- rB3in[["ctrls"]]$methodAgg
  }

  if (missing(pullAgg)){
    pullAgg <- rB3in[["ctrls"]]$pullAgg
  }

  if (missing(timestep)){
    timestep <- metaD$timestep
  }

  ######## trim unwanted timestamps and vars #######

  outs.idElToModify <- idElToModify(rB3in, startDate, endDate, varNames)

  # decompose the list
  colLocs <- outs.idElToModify[[2]]
  colLocs[1] <- TRUE # make sure datetime is preserved

  # cull the unwanted variables
  rB3trim <- rB3in

  DFs <- c("srcDF","qcDF","logDF")

  for (f in DFs) {
    # trim rows
  rB3trim[[f]] <- rB3trim[[f]][rB3trim[[f]]$DateTime >= as.POSIXct(startDate) & rB3trim[[f]]$DateTime <= as.POSIXct(endDate),]

     # trim cols
  rB3trim[[f]] <- rB3trim[[f]][,which(colLocs)]
      }
        # trim the ctrls variables to match the new DF cols
        rB3trim[["ctrls"]] <- rB3trim[["ctrls"]][colLocs,]

        # remove any NA rows
        rB3trim[["ctrls"]] <- rB3trim[["ctrls"]][!is.na(rB3trim[["ctrls"]]$measVar),]

  ######## aggregation #######


  rB3out <- rB3trim

  if (doAgg == TRUE) {

  # vars to aggregate
  varList <- colnames(rB3trim[["srcDF"]][,2:ncol(rB3trim[["srcDF"]])])

  if (aggAll == FALSE) {
    DFs <- "qcDF"
  } else {
    DFs <- c("srcDF","qcDF")
  }

    for (f in DFs) {

      aggDF <- rB3trim[[f]]
      aggDF$DateTime <- z_WRG_regDates(aggDF, timestep = timestep)
     #### agg each var one by 1, adding to a df

     # initialise with first variable

      dt_agg <- unique(aggDF$DateTime)

      for (i in 1:length(varList)) {

       aggVar <- data.table(aggDF[,c("DateTime",varList[i])])

       aggVar <- aggTS(aggVar,
                       timestep = timestep,
                       FUN = methodAgg[i],
                       pullAgg = pullAgg[i])

       dt_agg <- cbind(dt_agg, aggVar[2])
     }
   }

  if (aggAll == FALSE) {
    rB3out[["srcDF"]] <- rB3out[["qcDF"]]
  }

  rB3out[["logDF"]] <- rB3out[["qcDF"]]
     rB3out[["logDF"]] [1:nrow(rB3out[["logDF"]]),2:ncol(rB3out[["logDF"]])] <- NA

  }

  return(rB3out)

}
