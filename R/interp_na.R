#' Interpolate na values
#'
#' Assign linearly interpolated numbers for na when na is repeated less than maxRep number of times.
#' 
#' @export
#' @param startDate start date
#' @param endDate endDate
#' @param varNames variable names or keywords
#' @param maxRep maximum repeated NA
#' @param logID assign log ID
#' @param plotPath plot figure of before and after
#' @keywords QA/QC
#' @examples newDF <- interp_na(DF_in, maxRep = 4)
#' 

interp_na <- function(rB3in, startDate, endDate, varNames, maxRep, logID, Reason, showPlot, savePlot){
  
  ######## set defaults ######## 
  if (missing(startDate)){
    startDate <- rB3in[["qcDF"]]$DateTime[1]
  }
  
  if (missing(endDate)){
    endDate <- rB3in[["qcDF"]]$DateTime[length(rB3in[["qcDF"]]$DateTime)]
  }
  
  if (missing(varNames)){
    varNames <- "All"
  }
  
  if (missing(maxRep)){
    maxRep <- 2
  }
  
  if (missing(showPlot)){
    showPlot <- FALSE
  }
  
  if (missing(savePlot)) {
    savePlot <- NULL
  }
  
  if (missing(logID)){
    logID <- NA
  }

  # write to the logKey
  writeLog(rB3in, logID, funName = "interp_na", Reason = "Linearly interpolated na values" )
  
  ######## end set defaults ######## 
  
  ######## find elements to modify ######## 
  
  # extract data
  DF_in <- rB3in[["qcDF"]]
  
  outs.idElToModify <- idElToModify(
    rB3in,
    startDate = startDate,
    endDate = endDate,
    varNames = varNames)
  
  # decompose the list
  rowLocs <- outs.idElToModify[[1]]
  rowLocsNums <- which(rowLocs)
  colLocs <- outs.idElToModify[[2]]
  colLocsNums <- which(colLocs)
  
  ######## end find elements to modify ######## 
  
  
  ########          ########
  ######## function ######## 
  
  for (i in 1:length(colLocsNums)){ #colLocsNums

    # DF_in column to modify
    thisDF <- data.frame(DF_in[rowLocsNums,colLocsNums[i]])
    
    ##### log of this column 1 #####
    thisDF_log <- thisDF
    thisDF_log[,1] <- NA
    
    ##### log of this column 1 #####
    
    
    ##### algorithm to identify NA chunks ##### 
    
    # find na
    is.na.thisDF <- !is.na(thisDF)
    
    # counts continous na
    
    # test my algorithm
    # aa <- c(NA,NA,0,0,NA,NA,NA,0)
    # bb <- matrix(!is.na(aa),nrow = 1) # base matrics (find non NAs)
    
    bb <- is.na.thisDF
    
    cc <- cumsum(c(bb)) # cumsum TRUE (i.e. non NA)
    dd <- ave(seq_along(bb), cc, FUN=seq_along) # count continuous NAs
    ee <- which(dd[-length(dd)] - dd[-1L] > 0) # locatoin where NA sequence finishes
    
    if (is.na(thisDF[nrow(thisDF),1])){
      ee <- c(ee,nrow(thisDF))
    }
    
    flip.bb <- bb[c(length(bb):1)] # do same from the other side
    flip.cc <- cumsum(c(flip.bb)) # cumsum TRUE (i.e. non NA)
    flip.dd <- ave(seq_along(flip.bb), flip.cc, FUN=seq_along) # count continuous NAs
    flip.ee <- length(bb) - which(flip.dd[-length(flip.dd)] - flip.dd[-1L] > 0) + 1 # locatoin where NA sequence starts
    flip.ee <- flip.ee[c(length(flip.ee):1)] # sort in order
    
    if (is.na(thisDF[1,1])){
      flip.ee <- c(1, flip.ee)
    }
    
    ##### end algorithm to identify NA chunks ##### 
    
    ##### interpolation ##### 
    if (!(nrow(thisDF) == sum(is.na(thisDF)))){ # do nothing if there are no data
      for (j in 1:length(ee)){

        if (flip.ee[j] == 1){
          
          # do not interpolate if this chunk starts from the beggining of data
          
        } else if (ee[j] == nrow(is.na.thisDF)){
          
          # do not interpolate if this chunk starts from the beggining of data
          
        } else if ((ee[j] - flip.ee[j]) <= maxRep){
          
          # linear interpolation
          approx.out <- approx(x = c(1,(ee[j]+1)-(flip.ee[j]-1)+1),
                               y = c(thisDF[flip.ee[j]-1,1], thisDF[ee[j]+1,1]),
                               xout = seq_along((flip.ee[j]-1):(ee[j]+1)),
                               method = "linear")
          
          # assign back
          thisDF[(flip.ee[j]-1):(ee[j]+1),1] <- approx.out$y
          
          
          ##### log of this column 2 #####
          if (!is.na(logID)){
            # thisDF_log[(flip.ee[j]):(ee[j]),1] <- logID
            rB3in[["logDF"]] [(flip.ee[j]):(ee[j]),colLocsNums[i]]  <- logID
          }
          ##### log of this column 2 #####
        }
      }
    }

    DF_in[rowLocsNums,colLocsNums[i]] <- thisDF

    # ##### log 2 #####
    # if (!is.na(logID)){
    #   thisLog[rowLocsNums,colLocsNums[i]] <- thisDF_log
    # }
    # ##### log 2 #####
    
    ##### end interpolation ##### 
  }
  
 
  ##### plots #######
  
  rB3plot <- rB3in
  
  rB3plot[["preDF"]] <- rB3plot[["qcDF"]]
  rB3plot[["qcDF"]] <- DF_in
  
  
  # generate plot, if specified
  
  if (showPlot == TRUE | !is.null(savePlot)) {
    prePostPlot(rB3plot,
                startDate,
                endDate,
                varNames = varNames,
                srcColour = 'grey',
                preColour = 'red',
                qcColour = 'blue',
                showPlot = showPlot,
                savePlot = savePlot,
                dpi = 200)
  }
  
  ##### end plots #######
  
  # return rB3 obj
  rB3in[["qcDF"]] <- DF_in
  
  return(rB3in)
  
  ######## end function ######## 
}
