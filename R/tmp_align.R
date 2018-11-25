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
#' @keywords QA/Qc
#' @export
#' @examples newDF <- tmp_align(rB3in, startDate, endDate, varNames, dTPerctile, wndSpdPerctile, logID, Reason, showPlot, savePlot)

tmp_align <- function(rB3in, metaD, startDate, endDate, varNames, dTPerctile, wndSpdPerctile, logID, Reason, showPlot, savePlot){
  # This function align temperature profiles by finding the times which are 
  # mixed and theoretically indicate identical temperatures
  
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
  
  if (missing(showPlot)){
    showPlot <- FALSE
  }
  
  if (missing(savePlot)) {
    savePlot <- NULL
  }
  
  if (missing(logID)){
    logID <- NA
  }
  
  # write to the logKey
  writeLog(rB3in, logID, funName = "interp_na", Reason = "Linearly interpolated na values" )
  ######## end defaults ########
  
  ######## function ########
  
  # write to the logKey
  writeLog(rB3in, logID, funName = "interp_na", Reason = "Linearly interpolated na values" )
  
  # identify the elements in the array
  outs.idElToModify <- idElToModify(
    rB3in,
    startDate = startDate,
    endDate = endDate,
    varNames = varNames)
  
  # decompose the list
  rowLocs <- outs.idElToModify[[1]]
  colLocs <- outs.idElToModify[[2]]
  
  # extract DF
  DF_in <- rB3in[["qcDF"]]
  
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
  
  rB3in[["logDF"]][rowLocs,colLocs] <- logID
    
  ##### plots #######
  
  rB3plot <- rB3in
  
  rB3plot[["preDF"]] <- rB3plot[["qcDF"]]
  rB3plot[["qcDF"]] <- DF_in
  
  
  # generate plot, if specified
  
  if (showPlot == TRUE | !is.null(savePlot)) {
    prePostPlot(rB3plot,
                startDate,
                endDate,
                varNames = varNames,
                srcColour = 'grey',
                preColour = 'red',
                qcColour = 'blue',
                showPlot = showPlot,
                savePlot = savePlot,
                dpi = 200)
  }
  
  ##### end plots #######
  
  # return rB3 obj
  rB3in[["qcDF"]] <- DF_in
  
  return(rB3in)
  
  
} # end function