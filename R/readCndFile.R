readCndFile <- function(condFilePath){
  # read csv file which contains variable specific condition files
  # condFilePath should be relative to the wrapper file (Run_ file)
  
  ##### FUNCTION #####
  
  # read the table. First column must be the variable name
  rawDataIn <- read.table(file = condFilePath, 
                          fill = TRUE,
                          sep = ",")
  
  # accepted row names 
  rowNameList <- c("Variables",
                   "Plot_labels", 
                   "maxVal", 
                   "minVal", 
                   "Filter_RoC", 
                   "maxRep", 
                   "Filter_outlier_window",
                   "Filter_outlier_SD", 
                   "Sensor_model", 
                   "Sensor_serial")
  
  #
  rowNameList_exist <- "Variables"
  ic = 1
  for (i in 1:length(rowNameList)){
    colLocForThisRow <- which(grepl(rowNameList[i],rawDataIn[,1],ignore.case = TRUE))
    
    if (length(colLocForThisRow) > 0) { # check if the list exist
      if (i == 1){
        outDF <- rawDataIn[colLocForThisRow,]
      } else {
        outDF <- rbind(outDF, rawDataIn[colLocForThisRow,])
        ic = ic + 1
        rowNameList_exist[ic] <- rowNameList[i]
      }
    }
    
  }
  
  row.names(outDF) <- rowNameList_exist
  colnames(outDF) = lapply(rawDataIn[1,], as.character)
  
  return(outDF)
  
} # end function