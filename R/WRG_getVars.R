#' Return working variable names for the dataset
#'
#' @export
#' @param rB3in rB3 object input
#' @param varNames list of variable names or keywords
#' @keywords data
#' @examples rB3getVars(rB3in = rB3in, varNames = 'TmpWtr')
#'
#'

rB3getVars <- function(rB3in,varNames){

  ######## set defaults ########
  if (missing(varNames)){
    varNames <- 'All'
  }

  # get start and end for
  startDate <- rB3in[["qcDF"]]$DateTime[1]
  endDate <- rB3in[["qcDF"]]$DateTime[1]

  # identify the elements in the array, to be modified
  outs.idElToModify <- idElToModify(rB3in, startDate = startDate, endDate = endDate, varNames = varNames)

  # decompose the list
  colLocs <- outs.idElToModify[[2]]
  colLocsNums <- which(colLocs)

return( colnames(rB3in[['srcDF']][,colLocsNums]) )

}
