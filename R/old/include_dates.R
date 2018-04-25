include_dates <-  function(DF_in, taskDetails) {
  ######## import functions ########
  source("modules/idElToModify.R")
  source("modules/mkTaskList.R")

  ######## makeVarsList ########
  argNames <- c("StartDate","EndDate")
  
  varArgs <- mkTaskList(taskDetails,argNames,0)
  
  ######## defaults ########
  if (is.na(varArgs$StartDate)){
    varArgs$StartDate <- DF_in$DateTime[1]
  }
  
  if (is.na(varArgs$EndDate)){
    varArgs$EndDate <- DF_in$DateTime[length(DF_in$DateTime)]
  }
  
  varArgs$Vars <- "All"
  
  ######## Expand ########
  timestep <- 15 
  
  # dates   <-
  #   data.frame(seq.POSIXt(
  #     as.POSIXct(
  #       taskDetails$StartDate,
  #       origin = "1970-01-01 00:00:00",
  #       tz = "Etc/GMT+12"
  #     ),
  #     as.POSIXct(
  #       taskDetails$EndDate,
  #       origin = "1970-01-01 00:00:00",
  #       tz = "Etc/GMT+12"
  #     ),
  #     timestep * 60
  #   ))
  
  dates   <-
    data.frame(seq.POSIXt(
      as.POSIXct(
        varArgs$StartDate,
        origin = "1970-01-01 00:00:00",
        tz = "Etc/GMT+12"
      ),
      as.POSIXct(
        varArgs$EndDate,
        origin = "1970-01-01 00:00:00",
        tz = "Etc/GMT+12"
      ),
      timestep * 60
    ))
  
  names(dates) <- "DateTime"
  
  DF_in <- merge(DF_in, dates, all.y = T)

  ######## Now trim ########
  
  # identify the elements in the array
  outs.idElToModify <- idElToModify(DF_in, varArgs)
  
  # decompose the list
  rowLocs <- outs.idElToModify[[1]]
  colLocs <- outs.idElToModify[[2]]
  
  # add NA to the elements
  DF_in <- DF_in[rowLocs,]
  
  return(list(DF_in,colLocs,rowLocs))
  
} # end function