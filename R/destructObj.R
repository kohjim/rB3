#' Decompose object structure and export desired content
#'
#' @export
#' @param dataIn
#' @param exportName default: qcDF
#' @param exportOpt default: 1.
#' 1: return specified content.
#' 2. return specified content and object type (1: rB3obj, 2 = data.frame)
#' @keywords system
#' @examples LF = aggTS(dataIn = myData)
#'
#'

destructObj <- function(dataIn,exportName,exportOpt){

  if (missing(exportName)){
    exportName <- "qcDF"
  }

  if (missing(exportOpt)){
    exportOpt <- 1
  }

  # if rB3object, extract "exportName" object
  if (class(dataIn) == "rB3object"){
    objType = 1

    nameLocator = attributes(dataIn)$names == exportName
    returnObj = rB3demo[nameLocator]

  } else {
    # of not rB3object, assume it is already a data frame and only contains desired data
    objType = 2

    returnObj = dataIn
  }


  # return requested data
  if (exportOpt == 1){

    return(as.data.frame(returnObj)) # currently as data.frame

  } else {

    return(list(as.data.frame(returnObj), objType)) # currently as data.frame

  }
}
