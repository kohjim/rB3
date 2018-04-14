deleteBadData <- function(DF_in, taskDetails) {
  # This function removes data in specified dates. Require standard log file inputs.
  #
  # Example of log file
  #
  # [Label to apply], [function], [start date], [end date]
  # All, Delete, 2007-08-29- 10:45:00, 2007-09-07- 11:15:00
  # TmpWtr, Delete,2008-08-02- 19:00:00, 2008-08-02- 19:00:00 
  #
  
  #@  Chris McBride: chris@limnotrack.com , 2018-03-13  @#
  #@  Kohji Muraoka: kohji.muraoka@gmail.com @#
  
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
    varArgs$Vars <- "All"
  }
  
  ######## function ########
  # identify the elements in the array

  outs.idElToModify <- idElToModify(DF_in, varArgs)
  
  # decompose the list
  rowLocs <- outs.idElToModify[[1]]
  colLocs <- outs.idElToModify[[2]]
  
  # add NA to the elements
  DF_in[rowLocs,colLocs] <- NA
  
  return(list(DF_in,colLocs,rowLocs))

} # end function