readTaskFile <- function(taskFileName){
  # user settings
  numberOfColumns = 20
  attributesList = c("latitude","longitude","timestep","dataFileName","condFileName")
  wranglingHeaderName <- "WranglingID"
  taskListHeaderName <- "TaskID"
  
  # read the table: despite the format, read comma delimited with numberOfColumns number of columns
  rawDataIn <- read.table(file = taskFileName, 
                    fill = TRUE,
                    sep = ",",
                    col.names = paste("column", 1:numberOfColumns, sep = "_"), 
                    stringsAsFactors = FALSE) #  quote="" to keep quotes
  
  ####### extracting control information #######
  
  userSettings <- data.frame("null")
  for (i in 1:length(attributesList)){
    userSettings[,i] <- data.frame(
      rawDataIn[which(grepl(attributesList[i],unlist(rawDataIn[,1]),ignore.case=TRUE)),2]
      )
    colnames(userSettings)[i] <- attributesList[i]
  }
  
  
  ####### creating task list #######
  # find QC_ID to start recording the task lists
  wranglingRawLoc <- which(grepl(wranglingHeaderName,unlist(rawDataIn[,1]),ignore.case=TRUE))
  
  # find QC_ID to start recording the task lists
  taskRawLoc <- which(grepl(taskListHeaderName,unlist(rawDataIn[,1]),ignore.case=TRUE))

  
  # create data frame which only contains task lists
  wranglingList <- data.frame(rawDataIn[c(wranglingRawLoc+1):c(taskRawLoc-1),])
  
  # reassign colum names for taskDF
  colnames(wranglingList) = lapply(rawDataIn[wranglingRawLoc,], as.character)
  
  
  # create data frame which only contains task lists
  taskList <- data.frame(rawDataIn[-c(1:taskRawLoc),])
  
  # reassign colum names for taskDF
  colnames(taskList) = lapply(rawDataIn[taskRawLoc,], as.character)
  
  return(list(userSettings,wranglingList,taskList))
}
