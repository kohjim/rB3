#' Buoy observation to data frame
#'
#' Trim dates and/or aggregate (to a single common timestep) an rB3 working dataframe.
#'     Use 'ctrls' to define how aggregation should be performed for each time-series(column).
#'
#' @export
#' @param rB3in rB3 object to be 'standardized'
#' @param startDate measurements prior to this time will be removed
#' @param endDate measurements after this time will be removed
#' @param varNames variables (columns) to be retained, select by full header name, key characters, or 'All'
#' @param timestep standardise (aggregate) to a common timestep (T/F; turn aggregation on or off)
#' @param methodAgg aggregation method; mean, median, sum, min, max, or circular (for averaging direction measurements in degrees)
#' @param pullAgg aggregate data from before/on new timestamp ('left'), either side of timestamp ('centre'), or on/after timestamp ('right')
#' @keywords fileIO
#' @examples stdDF <- rB3stdze(rB3in = rawDF, varNames = wqVars, metaD = metaD, startDate = '2016-07-01 00:00:00', endDate = '2018-06-30 23:45:00', doAgg = TRUE, methodAgg = ctrls$methodAgg, pullAgg = ctrls$pullAgg)
#'
#'


fastStdze <- function(rB3in,startDate,endDate, varNames, timestep, methodAgg, pullAgg){
  
  ######## defaults ########
  if (missing(startDate)){
    startDate <- min(rB3in$DateTime)
  }
  
  if (missing(endDate)){
    endDate <- max(rB3in$DateTime)
  }
  
  if (missing(timestep)){
    timestep <- NULL
    doAgg <- FALSE
    
  } else {
    doAgg <- TRUE
    timestep <- timestep
  }
  
  if (missing(methodAgg)){
    methodAgg <- rB3in[["ctrls"]]$methodAgg
  }
  
  if (missing(pullAgg)){
    pullAgg <- rB3in[["ctrls"]]$pullAgg
  }
  
  if (missing(timestep)){
    timestep <- metaD$timestep
  }
  
  ######## trim unwanted timestamps and vars #######
  
  outs.idElToModify <- idElToModify(rB3in, startDate, endDate, varNames)
  
  # decompose the list
  colLocs <- outs.idElToModify[[2]]
  colLocs[1] <- TRUE # make sure datetime is preserved
  
  # cull the unwanted data
  rB3trim <- rB3in
  
  DFs <- c("srcDF","qcDF","logDF")
  
  for (f in DFs) {
    
    rB3trim[[f]] <- rB3trim[[f]][
      rB3trim[[f]]$DateTime >= as.POSIXct(startDate) &
        rB3trim[[f]]$DateTime <= as.POSIXct(endDate),
      ]
    
    rB3trim[[f]] <- rB3trim[[f]][,which(colLocs)]
  }
  
  
  # trim the ctrls variables to match the new DF cols
  rB3trim[["ctrls"]] <- rB3trim[["ctrls"]][colLocs,]
  
  
  ######## aggregation #######
  
  rB3out <- rB3trim
  
  if (doAgg == TRUE) {
    
    #create dates
    dates <- data.frame(
      seq.POSIXt(from = as.POSIXct(startDate),
                 to = as.POSIXct(endDate),
                 by = timestep * 60))
    names(dates) <- "DateTime"
    
    # vars to aggregate
    varList <- colnames(rB3trim[["srcDF"]][,2:ncol(rB3trim[["srcDF"]])])
    
    for (f in c("srcDF","qcDF")) {
      
      aggDF <- rB3trim[[f]]
      
      rB3out[[f]] <- dates
      
      for (i in 1:length(varList)) {
        
        aggDF <- rB3trim[[f]] [,c(1,i+1)]
        
        # set new timestamps for all measurements
        if (pullAgg[i] == 'left') {
          aggDF$DateTime <- lubridate::floor_date(
            aggDF$DateTime + (timestep * 60 -1),
            paste0(timestep,' min'))
          
        }  else if (pullAgg[i] == 'centre') {
          aggDF$DateTime <- lubridate::floor_date(
            aggDF$DateTime + (timestep * 30 -1),
            paste0(timestep,' min')
          )
          
        }  else if (pullAgg[i] == 'right') {
          aggDF$DateTime <- lubridate::floor_date(
            aggDF$DateTime + (timestep * 30 -1),
            paste0(timestep,' min')     )
          
        }  else {
          aggDF$DateTime <- lubridate::floor_date(
            aggDF$DateTime - (timestep * 60 + 1),
            paste0(timestep,' min')
          )
        }
        
        
        if (methodAgg[i] == 'mean') { 
          funAgg <- function(x){mean(x)}
          
        }  else if (methodAgg[i] == 'median') {
          funAgg <- function(x){median(x)}
          
        }  else if (methodAgg[i] == 'sum') {
          funAgg <- function(x){sum(x)}
          
        }  else if (methodAgg[i] == 'max') { 
          funAgg <- function(x){max(x)}
          
        }  else if (methodAgg[i] == 'min') {
          funAgg <- function(x){min(x)}
          
        }  else if (methodAgg[i] == 'circular') {
          funAgg <- function(x){
            mean(circular::circular(x, units = "degrees", modulo = '2pi'))
          }
          
        }  else {
          funAgg <- function(x)  {mean(x)}
        }
        
        
        # browser()
        dt = data.table(aggDF)
        
        # if (is.data.table(dt)) {
        #   print("yes")
        #   print(c(names(dt)[2]))
        #   print(c(names(dt)[1]))
        # } else {print("no")}
        
        # return(dt)
        # names(dt) <- c('x','y')
        # 
        # myDT <- data.table(x=rep(c("a","b","c"),each=3), y=c(1,3,6))
        # dt[, y := -y]
        
        # browser()
        aggVals = dt[ , funAgg(dt[[2]]), by = dt[[1]]]
        
        aggVals <- aggregate(
          aggDF[,2],
          by = list(aggDF$DateTime),
          FUN = funAgg)
        # 
        # browser()
        # names(aggVals2) <- 'DateTime'
        aggVals <- merge(dates, aggVals, all.x = TRUE)
        
        rB3out[[f]][,varList[i]] <- aggVals[,2]
        
      }
      
    }
    
    rB3out[["logDF"]] <- rB3out[["srcDF"]]
    rB3out[["logDF"]][1:nrow(rB3out[["logDF"]]),
                      2:ncol(rB3out[["logDF"]])] <- NA
    
  }
  
  return(rB3out)
  
}
