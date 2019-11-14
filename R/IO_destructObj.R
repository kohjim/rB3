#' Get qc data - private functions
#'
#' Export qc data regardless of rB3obj or data.table
#'
#' @export
#' @param dataIn data input
#' @param exportName default: qcDF
#' @param exportOpt default: 1.
#' 1: return specified content.
#' 2. return specified content and object type (1: rB3obj, 2 = data.frame)
#' @keywords system
#' @examples myData = destructObj(rB3data)
#' @examples myData = destructObj(myDF)
#' @examples objType = unlist(destructObj(rB3data,exportOpt = 2)[2])
#'

.destructObj <- function(dataIn,exportName,exportOpt){

  if (missing(exportName)){
    exportName <- "qcDF"
  }

  if (missing(exportOpt)){
    exportOpt <- 1
  }

  # if rB3object, extract "exportName" object
  if (sum(attr(a,"data.type") == "rB3obj")){
    objType = 1

    # find the data in the object structure
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
