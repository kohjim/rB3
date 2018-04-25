filter_stall <-  function(DF_in, taskInf) {
  ######## import functions ########
  source("modules/idElToModify.R")
  source("modules/findModuleParams.R")
  source("modules/readCndFile.R")
  source("modules/mkTaskList.R")
  
  ######## makeVarsList ########
  argNames <- c("StartDate","EndDate","VarNames","filename","numRepeats")
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
  varList <- c("numRepeats") # number of points being stuck in the same number
  
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
    numRepeats <- varArgs$numRepeats
  }
  
  # identify the elements in the array
  outs.idElToModify <- idElToModify(DF_in, varArgs)
  
  # decompose the list
  # rowLocs <- outs.idElToModify[[1]]
  colLocs <- outs.idElToModify[[2]]
  colLocsNums <- which(colLocs)
  
  # prepare DF to record all row locs being modified
  rowLocs <- DF_in[colLocs]
  rowLocs[] <- FALSE
  
  for (i in 1:length(colLocsNums)){
    # name of the column to be changed
    thisColName <- DFColNames[colLocsNums[i]]
    
    if (isUseCndFile){
      
      # identify condition file condition information locations
      cndColLocNum <- which(grepl(thisColName,colnames(cnd),ignore.case = TRUE))
      cndRowLocNum <- which(grepl(varList[1],rownames(cnd),ignore.case = TRUE))
      
      # identify conditions
      numRepeats <- cnd[cndRowLocNum,cndColLocNum]

    } else {

    }
    
    
    ##### algorithm to identify repeats and remove ##### 
    
    ## DF for proof of concepts ##
    # DF_in<- data.frame(ts = 1: 10, val = c(1,1,1,2,1,2,2,2,2,1)) 
    # colLocsNums <- c(1,2) 
    # i <- 2 
    # numRepeats <- 3 
    
    # DF_in column to modify
    thisDf <- data.frame(DF_in[,colLocsNums[i]])
    
    # compare with the next element and if true assign same ID
    neighbourComp <- with(thisDf, 
                          cumsum(
                            c(TRUE,
                              thisDf[-1L,1] != thisDf[-length(thisDf[,1]),1])
                          ))
    # count each repeats
    repeatCounts <- ave(seq_along(thisDf[,1]), 
                        neighbourComp, FUN=seq_along)
    
    # find repeats that exceeded use set max repeats
    excessRepeatLocs <- which(repeatCounts >= numRepeats)
    
    # identify all sequence locations that contains too many repeats
    thisDf$rowsToChange <- FALSE
    for (j in 1:length(excessRepeatLocs)){
      thisDf$rowsToChange[
        neighbourComp == neighbourComp[excessRepeatLocs[j]]] <- TRUE
    }
    
    # replace by NA
    DF_in[thisDf$rowsToChange,colLocsNums[i]] <- NA
    
    rowLocs[,i] <- thisDf$rowsToChange[]
  }

  
  return(list(DF_in,colLocs,rowLocs))
  
} # end function