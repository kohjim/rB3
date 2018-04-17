#' Buoy observation to data frame
#'
#' Import csv data into data frame
#' 
#' @export
#' @param filePath file path
#' @param metaD meta data
#' @param useHydroYear expand dataset to hydroyear limits
#' @keywords fileIO
#' @examples myDF <- bobs2df("Rotorua.bobs",metaD, useHydroYear = TRUE)
#' 
#' 

bobs2df <- function(filePath,metaD,useHydroYear){
  
  DF_in <-
    read.csv(filePath, header = TRUE)
  
  DF_in$DateTime <-
    as.POSIXct(DF_in$DateTime,
               origin = "1970-01-01 00:00:00",
               format = "%Y-%m-%d %H:%M:%S",
               tz = "Etc/GMT+12")
  
  timestep <- metaD$timestep
  
  ######## read file ######## 
  
  DF_in <-
    read.csv(filePath, header = TRUE)
  
  DF_in$DateTime <-
    as.POSIXct(DF_in$DateTime,
               origin = "1970-01-01 00:00:00",
               format = "%Y-%m-%d %H:%M:%S",
               tz = "Etc/GMT+12")
  
  
  ######## expand dataframe to full hydrological years ######## 
  
  firstDate <- DF_in$DateTime[1]
  lastDate <- DF_in$DateTime[length(DF_in$DateTime)]

  YrStart <- as.numeric(format(as.Date(firstDate), "%Y"))
  YrEnd <- as.numeric(format(as.Date(lastDate), "%Y"))
  
  MthStart <- as.numeric(format(as.Date(firstDate), "%m"))
  MthEnd <- as.numeric(format(as.Date(lastDate), "%m"))
  
  if (useHydroYear){
    
    YrStart <- ifelse(MthStart < 7, YrStart - 1, YrStart)
    
    YrEnd   <- ifelse(MthEnd < 7, YrEnd, YrEnd + 1)
    
    dates   <-
      data.frame(
        seq.POSIXt(
          as.POSIXct(
            paste0(YrStart, "-07-01 00:00:00"),
            origin = "1970-01-01 00:00:00",
            tz = "Etc/GMT+12"
          ),
          as.POSIXct(
            paste0(YrEnd, "-06-30 23:45:00"),
            origin = "1970-01-01 00:00:00",
            tz = "Etc/GMT+12"
          ),
          timestep * 60
        )
      )
    
    names(dates) <- "DateTime"
    
    DF_in <- merge(DF_in, dates, all.y = T)
    
  } else {

    dates   <-
      data.frame(seq.POSIXt(
        as.POSIXct(
          paste0(YrStart, "-07-01 00:00:00"),
          origin = "1970-01-01 00:00:00",
          tz = "Etc/GMT+12"
        ),
        as.POSIXct(
          paste0(YrEnd, "-06-30 23:45:00"),
          origin = "1970-01-01 00:00:00",
          tz = "Etc/GMT+12"
        ),
        timestep * 60
      ))
    
    names(dates) <- "DateTime"
    
    DF_in <- merge(DF_in, dates, all.y = T)
  }
  
}