findModuleParams <- function(taskInf,paramNameToFind,isRemoveSpace){

  tryCatch({
    paramLoc <- which(grepl(paramNameToFind, as.list(taskInf),ignore.case = TRUE))
    
    if (length(paramLoc) > 1){
      paramLoc <- paramLoc [length(paramLoc)]
    }
    paramContents <- strsplit(as.character(taskInf[paramLoc]),"=")
    outVar <- paramContents[[1]][2]
    
    if (missing(isRemoveSpace) | isRemoveSpace){
      # remove spaces by default
      outVar <- gsub(" ", "", outVar, fixed = TRUE)
    }
    
    return(outVar)
    
  }, error = function(e) {
    return(NA)
    
  })
  
}