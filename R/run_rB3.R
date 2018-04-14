run_rBDAC <- function(taskFileName){
  
  # usage
  # run_rBDAC("Rotorua_test.tsk")
  
  ###### library ######
  # 
  # tryCatch({
  #   library(tidyverse)
  # },  error = function(e) {
  #   ans <- menu(c("Yes", "No"), title="Do you want to install tidyverse package?")
  #   ifelse(ans == 1,install.packages("tidyverse"),stop("Stopped because some packages are unavailable"))
  # })
  
  ###### import functions ######
  source("modules/plotRawData.R")
  
  # task functions
  source("modules/deleteBadData.R")
  source("modules/tmp_align.R")
  source("modules/exclude_vars.R")
  source("modules/include_vars.R")
  source("modules/include_dates.R")
  source("modules/filter_max.R")
  source("modules/filter_min.R")
  source("modules/filter_stall.R")
  
  # system functions
  source("modules/returnFunctionName.R")
  source("modules/readCsvToDF.R")
  source("modules/readTaskFile.R")
  source("modules/readCndFile.R")
  source("modules/findModuleParams.R")
  source("modules/plotBeforeAfter.R")
  source("modules/mkLongOutDF.R")
  source("modules/mkTaskList.R")
  
  # dev setting
  isPlotBeforeAfter <- TRUE
  
  ###### import task file ###### 
  Outs_readTaskFile <- readTaskFile(taskFileName)
  userSettings <- Outs_readTaskFile[[1]] 
  wranglingList <- Outs_readTaskFile[[2]]
  taskList <- Outs_readTaskFile[[3]]
  
  # make this global
  assign("global_userSettings", Outs_readTaskFile[[2]], envir = .GlobalEnv) 
  
  dataFilePath <- paste0("../data/",userSettings$dataFileName)
  timestep <- as.numeric(as.character(userSettings$timestep))
  
  ###### plot raw data? ######
  isPlotRaw = 0;
  if (isPlotRaw){
    plotRawData(site, dataFileName, timestep)
  }
  
  ###### import data   ###### 
  DF_in <- readCsvToDF(dataFilePath,timestep,1) 
  
  # backup of the original import (maybe for later plotting)
  DF_bak <- DF_in 
  
  # long format
  # DF_long <- gather(DF_in, var, value, 2:(ncol(DF_in)))
  
  ###### wrangling ###### 
  for (i in 1:nrow(wranglingList)){
    # browser()
    # run assigned task by returnTaskName.R
    outs <- get(returnFunctionName(wranglingList$TaskName[i]))(DF_in, wranglingList[i,])

    # distribute task outputs
    DF_out <-outs[[1]] # DF output
    colsAffected <- outs[[2]] # affected column numbers (in DF_in format)
    rowsAffected <- outs[[3]] # affected column numbers (in DF_in format)  
    
    # DF_long <- mkLongOutDF(DF_long,DF_out,colsAffected,rowsAffected)
    
    # browser()
    # plot before and after
    if (isPlotBeforeAfter){
      plotBeforeAfter(DF_in,taskList[i,],DF_out,colsAffected,rowsAffected)
    }
    
    # replace DF_in with new DF_out for next task
    DF_in <- DF_out
  }

  ###### QAQC ###### 
  
  # for each task, run what need to do
  for (i in 1:nrow(taskList)){
    # for (i in 9){
    
    # run assigned task by returnTaskName.R
    outs <- get(returnFunctionName(taskList$TaskName[i]))(DF_in, taskList[i,])

    # distribute task outputs
    DF_out <-outs[[1]] # DF output
    colsAffected <- outs[[2]] # affected column numbers (in DF_in format)
    rowsAffected <- outs[[3]] # affected column numbers (in DF_in format)  
    
    # DF_long <- mkLongOutDF(DF_long,DF_out,colsAffected,rowsAffected)
    
    # browser()
    # plot before and after
    if (isPlotBeforeAfter){
      plotBeforeAfter(DF_in,taskList[i,],DF_out,colsAffected,rowsAffected)
    }
    
    # make log file
    # a <- data.frame(na,2,3,4)
    # b <- data.frame(2,3,4,5)
    # c <- paste(a,b, sep=",")
    # gsub("NA,", "", c, fixed = TRUE)
    
    # replace DF_in with new DF_out for next task
    DF_in <- DF_out
  }
  
}