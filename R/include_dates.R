#' @export

include_dates <-  function(DF_in, metaD, startDate, endDate) {

  ######## defaults ########
  if (missing(startDate)){
    startDate <- DF_in$DateTime[1]
  }
  
  if (missing(endDate)){
    endDate <- DF_in$DateTime[length(DF_in$DateTime)]
  }
  
  VarNames <- "All"
  
  timestep <- metaD$timestep
  
  ######## Expand dates ########

  dates   <-
    data.frame(
      seq.POSIXt(
        as.POSIXct(
          startDate,
          origin = "1970-01-01 00:00:00",
          tz = "Etc/GMT+12"
        ),
        as.POSIXct(
          endDate,
          origin = "1970-01-01 00:00:00",
          tz = "Etc/GMT+12"
        ),
        timestep * 60
      )
    )
  
  names(dates) <- "DateTime"
  
  DF_in <- merge(DF_in, dates, all.y = T)

  ######## Now trim ########
  
  # identify the elements in the array
  outs.idElToModify <- idElToModify(DF_in, startDate, endDate, VarNames)
  
  # decompose the list
  rowLocs <- outs.idElToModify[[1]]
  colLocs <- outs.idElToModify[[2]]
  
  # add NA to the elements
  DF_in <- DF_in[rowLocs,]
  
  return(DF_in)
  
} # end function