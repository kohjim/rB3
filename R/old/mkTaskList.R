mkTaskList <- function(taskRow,parameterNames,isRemoveSpace){
  # e.g. mkTaskList(taskList[1,],c("startDates","endDates"),0)
  # removespace = 0 for dates
  
  source("modules/findModuleParams.R")
  
  outList <- list()
  for (i in 1:length(parameterNames)){
    if (!hasArg(isRemoveSpace)){
      isRemoveSpace = 1
    }
    outList[[parameterNames[i]]] <- findModuleParams(taskRow,parameterNames[i],isRemoveSpace)
  }
  
  return(outList)
}