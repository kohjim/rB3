#' read a csv file with colums(dateTime, depth, varIDs, values) and merge exisiting HR dataset
#' 
#' @export
#' @param dataIn pre-existing data input; either data frame or rB3 object
#' @param filePath file path of the low resolution csv file
#' @param varNames variable names of low resolution input that you want to import 
#' @param suffix suffix you want to add to the low resolution input varNames
#' @param sparseMethod method of interpolation
#' @param minZ minimum depth to be averaged
#' @param maxZ mazximum depth to be averaged
#' @keywords fileIO
#' @examples sparseLR(dataIn = rB3in, filePath = "FLD_LakeRotorua.csv")
#' @examples sparseLR(dataIn = rB3in, filePath = "FLD_LakeRotorua.csv", varNames = "DO", suffix = "_FLD", minZ = 0, maxZ = 5)
#' 

sparseLR <- function(dataIn,filePath,varNames,suffix,sparseMethod,minZ,maxZ){

  # get LR object
  LR <- readLong(filePath)
  
  ######## defaults ########
  if (missing(varNames)){
    varNames <- LR$varNames
    
  } else if (varNames == "All"){
    varNames <- LR$varNames
  }
  
  if (missing(suffix)){
    suffix <- NULL
  }
  
  if (missing(sparseMethod)){
    sparseMethod <- 1
  }
  
  if (missing(minZ)){
    minZ <- -Inf
  }
  
  if (missing(maxZ)){
    maxZ <- Inf
  }
  
  ######## defaults ########
  
  ######## function ########
  
  
  
  # back up
  dataIn_bak <- dataIn
  
  ## get varNames and TS from pre-existing data
  
  if (is.data.frame(dataIn)){  
    # in case input was a data frame
    DF_HF <- dataIn
    
  } else { 
    # otherwise it has to be an rB3 object
    DF_HF <- dataIn[["qcDF"]]
  }

  # get TS as Data.Frame
  TS <- DF_HF[, "DateTime", drop=FALSE]
  
  ## for each LR varNames
  for (i in varNames){
    
    #### define new varName ###
    if (!is.null(suffix)){
      
      # make new var name according to suffix given
      newName <- paste0(i,suffix)
      
    } else if(!sum(colnames(DF_HF) == i)){ # sane varName exist?
      
      # if suffix was not specified and var name already exist, try using suffix "_LR"
      newName <- paste0(i,"_LR")
      
      # error if _LR exists
      if (sum(colnames(DF_HF) == newName)){
        browser()
        stop(
          paste0(
            newName,
            " exists in the rB3 object. Please remove the var or specify different suffix."
          )
        )
      }
    }
    
    # extract data from LR obj
    thisLR <- LR[[i]][
      LR[[i]][["DEPTH"]] >= minZ &
        LR[[i]][["DEPTH"]] <= maxZ &
        LR[[i]][["DateTime"]] >= min(TS[,]) &
        LR[[i]][["DateTime"]] <= max(TS[,]),
      c("DateTime","value")]
    
    # aggregate depths by averaging
    thisLR_agg <- aggregate(
      thisLR[,2],
      by = list(thisLR$DateTime),
      FUN = mean,
      na.action = na.omit
    )
    colnames(thisLR_agg) <- c("DateTime",i)
    
    # sparse data into HF data
    if (sparseMethod == 1){ # fill gap as NA
      
      this_LR_sparsed <- merge(
        TS,
        thisLR_agg,
        by = "DateTime",
        all.x = T,
        all.y = F
      )
      
    }
    
    # make output
    if (is.data.frame(dataIn)){  
      # in case input was a data frame
      dataIn[newName] <- this_LR_sparsed[,2]
      
    } else { 
      # otherwise it has to be an rB3 object
      dataIn <- varWrangle(dataIn,newName,'add')
      dataIn[["qcDF"]][newName] <- this_LR_sparsed[,2]
      dataIn[["LR"]] <- LR
    }
    
  }
  
  return(dataIn)
}