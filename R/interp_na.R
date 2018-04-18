#' Interpolate na values
#'
#' Assign linearly interpolated numbers for na when na is repeated less than maxRep number of time.
#' 
#' @export
#' @param DF_in data frame input
#' @param startDate start date
#' @param endDate endDate
#' @param varNames variable names or keywords
#' @param maxRep maximum repeated NA
#' @param logID assign log ID
#' @param plotPath plot figure of before and after
#' @keywords QA/QC
#' @examples newDF <- custom_fun_template_1(DF_in,metaD,startDate,endDate,varNames,logID,plotPath)
#' 

interp_na <- function(DF_in, metaD, startDate, endDate, varNames,logID, plotPath, maxRep){
  
  ######## log making 1 ######## 
  # check if DF is a list (i.e. with log)
  # default: input log does not exist
  
  log_exist <- FALSE
  inLog <- NULL
  
  if (!is.data.frame(DF_in)){
    inLog <- DF_in[[2]]
    outLog <- inLog
    log_exist <- TRUE
    
    DF_in <- DF_in[[1]]
  }
  
  DF_bak <- DF_in # to be used in plotDiff
  
  if (missing(logID)){
    logID <- NA
  } else {
    thisLog <- DF_in
    thisLog[,-1] <- NA
  }
  ######## end log making 1 ######## 
  
  ######## set defaults ######## 
  if (missing(startDate)){
    startDate <- DF_in$DateTime[1]
  }
  
  if (missing(endDate)){
    endDate <- DF_in$DateTime[length(DF_in$DateTime)]
  }
  
  if (missing(varNames)){
    varNames <- "All"
  }
  
  if (missing(plotPath)){
    plotPath <- NULL
  }
  
  if (missing(maxRep)){
    maxRep <- 2
  }
  
  ######## end set defaults ######## 
  
  
  
  ######## find elements to modify ######## 
  
  outs.idElToModify <- idElToModify(DF_in,
                                    startDate = startDate,
                                    endDate = endDate,
                                    varNames = varNames)
  # decompose the list
  rowLocs <- outs.idElToModify[[1]]
  rowLocsNums <- which(rowLocs)
  colLocs <- outs.idElToModify[[2]]
  colLocsNums <- which(colLocs)
  
  ######## find elements to modify ######## 
  
  
  
  ########          ########
  ######## function ######## 

  
  for (i in 1:length(colLocsNums)){#colLocsNums

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
    if (!(nrow(thisDF) == nrow(is.na.thisDF))){ # do nothing if there are no data
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
            thisDF_log[(flip.ee[j]):(ee[j]),1] <- logID
          }
          ##### log of this column 2 #####
        }
      }
    }

    DF_in[rowLocsNums,colLocsNums[i]] <- thisDF
    
    ##### log 2 #####
    if (!is.na(logID)){
      thisLog[rowLocsNums,colLocsNums[i]] <- thisDF_log
    }
    ##### log 2 #####
    
    ##### end interpolation ##### 
  }
  
 
  ######## end function ######## 
  ########              ########
  
  
  
  ######## save plot diff ######## 
  if (!is.null(plotPath)){  
    plotDiff(DF_bak, DF_in,
             colNum = colLocsNums,
             plotPath = plotPath,
             custom_dpi = 150,
             taskName = "taskName")   ######### Change name here (figure's title will contain this info)
  }
  ######## save plot diff ######## 
  
  
  ######## return with or without Log ########
  if (!is.na(logID) | log_exist){
    
    return(list(DF_in,outLog))
    
  } else {
    
    return(DF_in)
  }
  ######## end return with or without Log ########
}
