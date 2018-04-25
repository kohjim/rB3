exclude_vars <-  function(DF_in, taskDetails) {
  ######## import functions ########
  source("modules/idElToModify.R")
  source("modules/mkTaskList.R")
  
  ######## makeVarsList ########
  argNames <- c("StartDate","EndDate","Vars")
  varArgs <- mkTaskList(taskDetails,argNames,0)
  # varArgs$Vars <- strsplit(as.character(varArgs$Vars),",")

  ######## defaults ########
  if (is.na(varArgs$StartDate)){
    varArgs$StartDate <- DF_in$DateTime[1]
  }

  if (is.na(varArgs$EndDate)){
    varArgs$EndDate <- DF_in$DateTime[length(DF_in$DateTime)]
  }

  if (is.na(varArgs$Vars)){
    varArgs$Vars <- na
  }
  
  ######## function ########
  # identify the elements in the array
  outs.idElToModify <- idElToModify(DF_in, varArgs)
  
  # decompose the list
  rowLocs <- outs.idElToModify[[1]]
  colLocs <- outs.idElToModify[[2]]

  # add NA to the elements
  DF_in[,which(colLocs)] <- NULL
  
  return(list(DF_in,colLocs,rowLocs))
  
} # end function