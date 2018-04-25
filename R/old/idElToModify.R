idElToModify <- function(DF_in, taskDetails){
  # retun location information TF information of the data from the task information
  # either taskDetails with full task information or character elements separated by spaces are required
  #
  # require taskDetails$StartDate taskDetails$EndDate taskDetails$Vars
  #
  # Usage
  # outs <- idElToModify(DF_in, taskDetails)
  #   rowLocs <- outs[[1]]
  #   colLocs <- outs[[2]]
  #
  # outs <- idElToModify(DF_in, "tmp DO")
  #   colLocs <- outs[[2]]
  #
# browser()
  # see if taskDetails$StartDate exists
  if (!is.null(taskDetails$StartDate)) {
    
    # convert dates into POSIX format
    if (!is.na(taskDetails$StartDate)){
      taskDetails$StartDate <-
        as.POSIXct(
          taskDetails$StartDate,
          origin = "1970-01-01 00:00:00",
          format = "%Y-%m-%d %H:%M:%S",
          tz = "Etc/GMT+12")
      } else {
          taskDetails$StartDate <- DF_in$DateTime[1]
      }
    
    if (!is.na(taskDetails$EndDate)){
    taskDetails$EndDate   <-
      as.POSIXct(taskDetails$EndDate,
                 origin = "1970-01-01 00:00:00",
                 format = "%Y-%m-%d %H:%M:%S",
                 tz = "Etc/GMT+12")
    } else {
      taskDetails$StartDate <- DF_in$DateTime[length(DF_in$DateTime)]
    }
    
    
    # identify rows affected by the change
    delRows <- DF_in$DateTime >= taskDetails$StartDate &
      DF_in$DateTime <= taskDetails$EndDate | 
      is.na(DF_in$DateTime)
    
    # identify columns affected by the task
    if (taskDetails$Vars == "All"){
      delCols <- !grepl("DateTime",unlist(labels(DF_in)[2]))
    } else {
      # find vars applied and separate them (delimiter <- space)
      targetVars <- gsub(" ", "", taskDetails$Vars, fixed = TRUE)
      colLabs <- unlist(base::strsplit(as.character(targetVars),",")) 
      delCols <- grepl(paste(colLabs, collapse = "|"),unlist(labels(DF_in)[2]),ignore.case = TRUE)
    }
  } else {
    # browser()
    # when inputs are characters
    delRows <- 0
    # outVar <- gsub(" ", "", outVar, fixed = TRUE)
    colLabs <- unlist(base::strsplit(as.character(taskDetails[[1]]),","))
    delCols <- grepl(paste(colLabs, collapse = "|"),unlist(labels(DF_in)[2]),ignore.case = TRUE)
  }
  return(list(delRows,delCols))
  
} # end function