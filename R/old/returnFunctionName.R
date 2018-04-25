returnFunctionName <- function(taskName){
  
  # give numeric ID to the task
  if (grepl(taskName, "delete", ignore.case = TRUE)){
    taskFunction <- "deleteBadData"
  } 
  
  else if (grepl(taskName, "tmp_align",ignore.case = TRUE)){
    taskFunction <- "tmp_align"
  } 
  
  else if (grepl(taskName, "exclude_vars",ignore.case = TRUE)){
    taskFunction <- "exclude_vars"
  } 
  
  else if (grepl(taskName, "inlude_vars",ignore.case = TRUE)){
    taskFunction <- "include_vars"
  } 
  
  else if (grepl(taskName, "include_dates",ignore.case = TRUE)){
    taskFunction <- "include_dates"
  } 
  
  else if (grepl(taskName, "Apply_eqn2",ignore.case = TRUE)){
    taskFunction <- "apply_eqn2"
  } 
  
  else if (grepl(taskName, "Drift",ignore.case = TRUE)){
    taskFunction <- "drift"
  } 
  
  else if (grepl(taskName, "Calc_DOmg",ignore.case = TRUE)){
    taskFunction <- "calc_DOSat"
  } 
  
  else if (grepl(taskName, "Filter_max",ignore.case = TRUE)){
    taskFunction <- "filter_max"
  } 
  
  else if (grepl(taskName, "Filter_min",ignore.case = TRUE)){
    taskFunction <- "filter_min"
  } 
  
  else if (grepl(taskName, "Filter_stall",ignore.case = TRUE)){
    taskFunction <- "filter_stall"
    
  } 
  
  else if (grepl(taskName, "Filter_outlier",ignore.case = TRUE)){
    taskFunction <- "filter_outlier"
  } 
  
  else if (grepl(taskName, "Radiation_max",ignore.case = TRUE)){
    taskFunction <- "Radiation_max"
  } 
  
  else if (grepl(taskName, "Sensor_model",ignore.case = TRUE)){
    taskFunction <- "Sensor_model"
  } 
  
  else if (grepl(taskName, "Sensor_serial",ignore.case = TRUE)){
    taskFunction <- "Sensor_serial"
  } 
  
  # error message
  else { 
    stop(paste0("Task name '", taskName, "' was not recognized by the program"))
  }
  
  return(taskFunction)
} # end function