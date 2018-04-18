#' Specify what variables to include
#'
#' This function specifies variables to be included for later changes.
#' 
#' @param DF_in data frame input
#' @param metaD metadata list
#' @param varNames list of variable names or keywords
#' @keywords wrangling
#' @export
#' @examples newDF <- include_vars(myDF,metaData,varNames = c("tmp","do","wnd","pH"))
#' 

include_vars <-  function(DF_in, metaD, varNames) {

  ######## defaults ########
  startDate <- NA
  endDate <- NA

  ######## function ########
  # identify the elements in the array
  outs.idElToModify <- idElToModify(DF_in, startDate, endDate, varNames)
  
  # decompose the list
  rowLocs <- outs.idElToModify[[1]]
  colLocs <- outs.idElToModify[[2]]
  colLocs[1] <- TRUE # make sure datetime is preserved

  # add NA to the elements
  DF_in[,-which(colLocs)] <- NULL
  
  return(DF_in)
  
} # end function