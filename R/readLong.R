#' read csv with colums(dateTime, depth, varIDs, values) and return list of unique varIDs
#' 
#' @export
#' @param filePath file path of source csv file, timestamps in first column as yyyy-mm-dd hh:mm:ss with header "DateTime"
#' @keywords fileIO
#' @examples readLong("Rotorua.csv")

readLong <- function(filePath){

  # load field file (should be fromat of DateTime, DEPTH, var, value)
  srcDF <- read.csv(filePath, 
                    header = T,
                    stringsAsFactors = FALSE)
  
  # rename in case 
  names(srcDF) <- c("DateTime", "DEPTH", "var", "value")
  
  srcDF$DateTime <-  as.POSIXct(srcDF$DateTime,
                                origin = "1970-01-01 00:00:00",
                                tz = "UTC")
  
  srcDF$value <- as.numeric(srcDF$value)
  
  
  # build outputs
  
  # empty list
  df_out <- list()
  
  # get unique var ids
  df_out$varNames <- as.character(unique(srcDF$var))
  
  # create vectors for each var kinds
  for (i in df_out$varNames ){
    df_out[[i]] <- as.data.frame(srcDF[srcDF[,3] == i,c(1,2,4)])
  }
  
  return(df_out)
  
  # end function
}