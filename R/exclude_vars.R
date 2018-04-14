#' Specify what variables to exclude
#'
#' This function specifies variables to be excluded for later changes.
#' @param DF_in data frame input
#' @param metaD metadata list
#' @param varNames list of variable names or keywords
#' @keywords wrangling
#' @export
#' @examples newDF <- exclude_vars(myDF,metaData,varNames = c("pH","wndDir"))
#' 

exclude_vars <-  function(DF_in, metaD, varNames) {
  
  ######## defaults ########
  startDate <- NA
  endDate <- NA
  
  ######## function ########
  # identify the elements in the array
  outs.idElToModify <- idElToModify(DF_in, startDate, endDate, varNames)
  
  # decompose the list
  rowLocs <- outs.idElToModify[[1]]
  colLocs <- outs.idElToModify[[2]]

  # add NA to the elements
  DF_in[,which(colLocs)] <- NULL
  
  return(DF_in)
  
} # end function