#' Automatically remove cross thermister observation difference
#'
#' This function assign NA to the specific dates range and variables
#'
#' @param rB3in data frame input
#' @param startDate start date
#' @param endDate endDate
#' @param varNames list of variable names or keywords (e.g. varNames = "tmpwtr")
#' @param dTPerctile percentile of temperature variation to determine mixed periods
#' @param wndSpdPerctile percentile of windspeed to make assure mixed periods (optional). wndspd values will be used
#' @param logID write an operation identifier to the log frames
#' @param Reason reason for hte logged changes (character)
#' @param showPlot display figure in plots window (TRUE/FALSE)
#' @param savePlot save figure to a path (TRUE/FALSE)
#' @param plotType 'All' = allvars on one plot, compare before and after, '1by1' = separate panel of before and after for each vasr
#' @keywords Lake_buoy_data
#' @export
#' @examples newDF <- tmp_align(rB3in, startDate, endDate, varNames, dTPerctile, wndSpdPerctile, logID, Reason, showPlot, savePlot)

  tmprAlign <- function(rB3in, metaD, startDate, endDate, varNames, dTPerctile, wndSpdPerctile, logID, Reason, showPlot, savePlot, plotType){
  # This function align sensor calibration by finding the times which are most similar
  # and theoretically should provide near-identical readings

  ######## defaults ########
  if (missing(startDate)){
    startDate <- rB3in[["qcDF"]]$DateTime[1]
  }

  if (missing(endDate)){
    endDate <- rB3in[["qcDF"]]$DateTime[length(rB3in[["qcDF"]]$DateTime)]
  }

  if (missing(varNames)){
    varNames <- "TmpWtr"
  }

  if (missing(logID)){
    logID <- "Align"
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

    if (missing(plotType)) {
      plotType <- 'All'
    }


  rB3new <- rB3in

  # write to the logKey
  writeLog(rB3new, logID, funName = "Align", Reason = Reason )
  ######## end defaults ########

  # identify the elements in the array
  outs.idElToModify <- idElToModify(
    rB3in,
    startDate = startDate,
    endDate = endDate,
    varNames = varNames)

  # decompose the list
  rowLocs <- outs.idElToModify[[1]]
  rowLocsNums <- which(rowLocs)
  colLocs <- outs.idElToModify[[2]]
  colLocsNums <- which(colLocs)

  # extract DF
  DF_in <- rB3new[["qcDF"]]

  if (!missing(wndSpdPerctile)){ # with wind
    # wind location
    outs.idElToModify <- idElToModify(
      rB3in,
      startDate,
      endDate,
      varNames = c("wndspd"))

    wndLocs <- outs.idElToModify[[2]]

    # assign data
    Temps <- DF_in[rowLocs,colLocs]
    Wspd <- DF_in[rowLocs,wndLocs]

    # location of data that has more than one temperature in the profile
    # TempsWithSomeDataLocs <- !is.infinite(apply(df_Temps, 1, max, na.rm = TRUE))

    # location of the data that has more than one temperature in the profile, and have wind data
    #  (locs in the original vector)
    dataLocs1  <- which(
      !is.infinite(apply(Temps, 1, max, na.rm = TRUE)) &
        !is.na(Wspd))

    # find usable data for mixed data detection
    goodTemps <- Temps[dataLocs1,]
    goodWspd <- Wspd[dataLocs1]

    # calculate temperature differences over different depths
    dTemps <- apply(goodTemps, 1, max, na.rm = TRUE) -
      apply(goodTemps, 1, min, na.rm = TRUE)

    # find the mixed events that has less than 20 percentile layer temperature
    # differences with over 90 percentile wind events
    #  (locs in dataLocs1 vector)
    mixedLocs_in_dataLocs1 <- which(
      dTemps < quantile(dTemps, probs = as.numeric(dTPerctile)) &
        goodWspd > quantile(goodWspd, probs = as.numeric(wndSpdPerctile))
    )

  } else { # without wind
    # assign data
    Temps <- DF_in[rowLocs,colLocs]

    # location of data that has more than one temperature in the profile
    # TempsWithSomeDataLocs <- !is.infinite(apply(df_Temps, 1, max, na.rm = TRUE))

    # location of the data that has more than one temperature in the profile, and have wind data
    #  (locs in the original vector)
    dataLocs1  <- which(
      !is.infinite(apply(Temps, 1, max, na.rm = TRUE))
      )

    # find usable data for mixed data detection
    goodTemps <- Temps[dataLocs1,]

    # calculate temperature differences over different depths
    dTemps <- apply(goodTemps, 1, max, na.rm = TRUE) -
      apply(goodTemps, 1, min, na.rm = TRUE)

    # find the mixed events that has less than 20 percentile layer temperature
    # differences with over 90 percentile wind events
    #  (locs in dataLocs1 vector)
    mixedLocs_in_dataLocs1 <- which(
      dTemps < quantile(dTemps, probs = dTPerctile)
    )
  }

  # data locatin in the original vector (locs in the original vector)
  mixedLocs <- dataLocs1[mixedLocs_in_dataLocs1]

  # use median of the temperature profile from the mixed events to be the most accurate temperature
  medianTemps <- matrix(0, nrow(Temps), 1) # same length as original file
  medianTemps[mixedLocs,] <- apply(Temps[mixedLocs,], 1, median, na.rm = TRUE)


  # iterate through columns to correct temperature observations
  # i <- 1
  for (i in 1:ncol(Temps)){
    # vector of this depth's temperature
    thisTemp_bak <- Temps[,i]

    # location of this temp's values with no NA (locs in the original vector)
    noNALocs <- which(!is.na(thisTemp_bak))

    # satisfy both mixedLocs and noNALocs (locs in the original vector)
    locsToMakeModel <- intersect(mixedLocs, noNALocs)

    if (is.integer(locsToMakeModel)){
      # satisfy both mixedLocs and noNALocs
      # goodTempToModel <- thisTemp_bak[intersect(mixedLocs, noNALocs)]

      # poly fit model
      x <- thisTemp_bak[locsToMakeModel]
      y <- medianTemps[locsToMakeModel,]
      myModel <- lm(y ~ poly(x, 2))

      # p <- poly(x, y, degree = 2)

      Temps[noNALocs,i] <- predict.lm(myModel,list(x=thisTemp_bak[noNALocs]))
    }
  }

  DF_in[rowLocs,colLocs] <- Temps

  ######## end function ########

  rB3new[["logDF"]] [rowLocs,colLocs] <- ifelse(is.na(rB3new[["logDF"]] [rowLocs,colLocs]),
                                                           logID,
                                                           paste0(rB3new[["logDF"]][rowLocs,colLocs], ' : ',logID ) )


   ##### plots #######
  rB3new[["hlDF"]] <- rB3new[["qcDF"]]
  rB3new[["qcDF"]] <- DF_in

  if (showPlot == TRUE | !is.null(savePlot)) {
  # generate plot, if specified

           ####### MAKE A FACETED GGPLOT ################

    if ( plotType == 'All') {

    plotQC <- rB3new[["qcDF"]][,c(1,colLocsNums)]
    # colnames(plotQC) <- c("DateTime",plotLabels[colLocsNums - 1])
    plotQC <- tidyr::gather(plotQC,var, value, 2:ncol(plotQC))
    plotQC$type <- "Aligned (new) data"

    plotSrc <- rB3new[["srcDF"]][,c(1,colLocsNums)]
    # colnames(plotSrc) <- c("DateTime",plotLabels[colLocsNums - 1])
    plotSrc <- tidyr::gather(plotSrc,var, value, 2:ncol(plotSrc))
    plotSrc$type <- "Unmodified source data"

    plotAll <- rbind(plotQC, plotSrc)

    # generate plot
    print(

      ggplot2::ggplot(plotAll) +

        ggplot2::geom_line(ggplot2::aes(x = DateTime, y = value, color = var), size = 0.2, na.rm = T) +

        ggplot2::ylab("Value") +
        ggplot2::xlab(NULL) +
        ggplot2::scale_x_datetime(labels = scales::date_format("%Y-%m"),
                                  # breaks = scales::date_breaks("1 years"),
                                  limits = c(min(plotAll$DateTime),max(plotAll$DateTime)),
                                  expand = c(0, 0)) +

        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 0, hjust = 0)) +
        ggplot2::theme_bw() +
        ggplot2::theme(legend.position = "bottom") +
        ggplot2::facet_wrap(~type, ncol = 1, scales = 'free_y')

     )
    } else {

    prePostPlot(rB3new,
                startDate,
                endDate,
                varNames = varNames,
                srcColour = 'grey',
                hlColour = 'red',
                qcColour = 'blue',
                showPlot = showPlot,
                savePlot = savePlot,
                dpi = 200)

    }

    if (!is.null(savePlot)) {

      ggplot2::ggsave(paste0(savePlot, rB3in[["metaD"]]$siteName,"_facet.png"),
                      height = 0.5 + 1.1 * length(unique(plotAll$var)),
                      width = 7.5,
                      dpi = dpi)

  }

    if (menu(c("Yes", "No"), title="Apply these changes?") == 1){

      # keep rB3new
      print('Changes have been applied')

      # ..or revert to rB3in
    } else {
      rB3new <- rB3in
      print ( 'Changes were not applied' )
    }

  }   # end plotting loop

  # return the modified rB3 object
  return(rB3new)

  ######## end function ########
  }
