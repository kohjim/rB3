tmp_align <- function(DF_in, taskDetails){
  # This function align temperature profiles by finding the times which are 
  # mixed and theoretically indicate identical temperatures
  #
  # Example of log file
  #
  # [Label to apply], [function], [start date], [end date]
  # TmpWtr, Align, 2007-08-29- 10:45:00, 2007-09-07- 11:15:00
  #
  # browser()
  ######## import functions ########
  source("modules/idElToModify.R")
  source("modules/findModuleParams.R")
  source("modules/mkTaskList.R")
  
  ######## makeVarsList ########
  argNames <- c("StartDate","EndDate","VarNames","dTPerctile","wndSpdPerctile")
  varArgs <- mkTaskList(taskDetails,argNames,0)
  varArgs$Vars <- varArgs$VarNames
  
  
  ######## defaults ########
  if (is.na(varArgs$StartDate)){
    varArgs$StartDate <- DF_in$DateTime[1]
  }
  
  if (is.na(varArgs$EndDate)){
    varArgs$EndDate <- DF_in$DateTime[length(DF_in$DateTime)]
  }
  
  if (is.na(varArgs$Vars)){
    varArgs$Vars <- "TmpWtr"
  }
  
  ######## function ########
  # identify the elements in the array
  outs.idElToModify <- idElToModify(DF_in, varArgs)
  
  # decompose the list
  rowLocs <- outs.idElToModify[[1]]
  colLocs <- outs.idElToModify[[2]]
  
  if (!is.na(varArgs$wndSpdPerctile)){ # with wind
    # wind location
    outs.idElToModify <- idElToModify(DF_in, as.data.frame("wndspd"))
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
      dTemps < quantile(dTemps, probs = as.numeric(varArgs$dTPerctile)) &
        goodWspd > quantile(goodWspd, probs = as.numeric(varArgs$wndSpdPerctile))
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
      dTemps < quantile(dTemps, probs = varArgs$dTPerctile)
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
    
    if (!is_empty(locsToMakeModel)){
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
  
  return(list(DF_in,colLocs,rowLocs))
  
} # end function