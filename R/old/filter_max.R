filter_max <-  function(DF_in, taskInf) {
  ######## import functions ########
  source("modules/idElToModify.R")
  source("modules/findModuleParams.R")
  source("modules/readCndFile.R")
  source("modules/mkTaskList.R")
  ######## makeVarsList ########
  argNames <- c("StartDate","EndDate","VarNames","filename","maxVal")
  varArgs <- mkTaskList(taskInf,argNames,0)
  varArgs$Vars <- varArgs$VarNames
  varArgs$filename <- gsub(" ", "", varArgs$filename, fixed = TRUE)
  # varArgs$Vars <- strsplit(as.character(varArgs$Vars),",")
  
  ######## defaults ########
  if (is.na(varArgs$StartDate)){
    varArgs$StartDate <- DF_in$DateTime[1]
  }
  
  if (is.na(varArgs$EndDate)){
    varArgs$EndDate <- DF_in$DateTime[length(DF_in$DateTime)]
  }
  
  if (is.na(varArgs$Vars)){
    varArgs$Vars <- "All"
  }

  ######## function ########
  # list of variable names
  varList <- c("maxVal")
  
  # col names for later use
  DFColNames <- colnames(DF_in[,])
  
  # get variables
  # out <- findModuleParams(taskInf,"filename")
  
  if (!is.na(varArgs$filename)){
    isUseCndFile <- TRUE
    condFilePath <- paste0("../data/",varArgs$filename)
    cnd <- readCndFile(condFilePath)
    
  } else {
    isUseCndFile <- FALSE
    maxVal <- varArgs$maxVal
  }
  
  # identify the elements in the array
  outs.idElToModify <- idElToModify(DF_in, varArgs)
  
  # decompose the list
  rowLocs <- outs.idElToModify[[1]]
  rowLocsNums <- which(rowLocs)
  colLocs <- outs.idElToModify[[2]]
  colLocsNums <- which(colLocs)
  
  for (i in 1:length(colLocsNums)){
    # name of the column to be changed
    thisColName <- DFColNames[colLocsNums[i]]
    
    if (isUseCndFile){
      
      # identify condition file condition information locations
      cndColLocNum <- which(grepl(thisColName,colnames(cnd),ignore.case = TRUE))
      cndRowLocNum <- which(grepl(varList[1],rownames(cnd),ignore.case = TRUE))
      
      # identify conditions
      maxVal <- cnd[cndRowLocNum,cndColLocNum]
      
      # find conditioned location in the DF_in
      rowsToChange <- which(DF_in[,colLocsNums[i]] > as.numeric(as.character(maxVal)))
      rowsToChange <- intersect(rowLocsNums,rowsToChange)
      
      # replace by NA
      DF_in[rowsToChange,colLocsNums[i]] <- NA
      
    } else {
      # find conditioned location in the DF_in
      rowsToChange <- which(DF_in[,colLocsNums[i]] > as.numeric(as.character(maxVal)))
      rowsToChange <- intersect(rowLocsNums,rowsToChange)
      
      # replace by NA
      DF_in[rowsToChange,colLocsNums[i]] <- NA
    }
  }

  return(list(DF_in,colLocs,rowLocs))
  
} # end function