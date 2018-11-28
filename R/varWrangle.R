#' Create or remove variables in an rB3 object
#'
#' Creat variables, delete variables, change order of variables, etc
#'
#' @param rB3in rB3 object input
#' @param varNames list of variable names or keywords
#' @param task one of: rm, add, or moveto
#' @param loc N: move or add varNames after original Nth variable
#' @keywords wrangling
#' @export
#' @examples rB3in <- varWrangle(rB3in, varNames = "tmpWtr", task = "sort")
#' @examples rB3in <- varWrangle(rB3in, varNames = "tmpWtr", task = "moveto", loc = 3)
#' @examples rB3in <- varWrangle(rB3in, varNames = "DOSat", task = "add")
#' @examples rB3in <- varWrangle(rB3in, varNames = "tmpWtr", task = "rm")

varWrangle <- function(rB3in, varNames, task, loc) {
  
  ######## defaults ########
  
  startDate <- rB3in[["qcDF"]]$DateTime[1]
  endDate <- rB3in[["qcDF"]]$DateTime[length(rB3in[["qcDF"]]$DateTime)]
  
  if (missing(varNames)){
    varNames <- "All"
  }
  
  if (missing(loc)){
    loc <- NULL
  }
  
  ######## check vars ########
  # identify the elements in the array, to be modified
  outs.idElToModify <- idElToModify(
    rB3in, 
    startDate = startDate,
    endDate = endDate, 
    varNames = varNames
  )
  
  # decompose the list
  colLocs <- outs.idElToModify[[2]]
  colLocsNums <- which(colLocs)
  
  ######## end check vars ########
  
  if (sum(colLocs) == 0){ # the specified varNames do not exist, add the VarNames
    
    if (missing(task)){
      task <- "add"
      
    } else if (task != "add") {
      stop("The specified varNames do not exist.")
      
    }
    
  } else { # if varNames exist and no task is specified, remove the varNames
    
    if (missing(task)){
      task <- "rm"
      print('Caution: task was automatically set as rm')
      
    } else if (task == "add") {
      stop("Some or all of the specified varNames already exist.")
      
    }
  }
  
  ######## end defaults ########
  
  ######## ######## ########
  ######## function ########
  ######## ######## ########
  
  orgNCol = ncol(rB3in[["srcDF"]])
  
  if (task == "add"){ # add new variables
    
    rB3in[["srcDF"]][,varNames] <- NA
    rB3in[["qcDF"]][,varNames] <- NA
    rB3in[["logDF"]][,varNames] <- NA
    rB3in[["ctrls"]][varNames,] <- NA
    
    newNCol = ncol(rB3in[["srcDF"]])
    
    # because ctrls have N-1 vars (without dateTime), adjust this complication..
    
    rB3in[["ctrls"]] <- rB3in[["ctrls"]][c(1:(orgNCol-1),(orgNCol+1):(newNCol),orgNCol),]
    
    # continue program
    
  } else if (task == "rm"){ # remove variables
    
    rB3in[["srcDF"]] <- rB3in[["srcDF"]][,!colLocs]
    rB3in[["qcDF"]] <- rB3in[["qcDF"]][,!colLocs]
    rB3in[["logDF"]] <- rB3in[["logDF"]][,!colLocs]
    rB3in[["ctrls"]] <- rB3in[["ctrls"]][!c(colLocs[2:orgNCol],FALSE),]
    
    print(paste0(paste0(varNames,collapse = ", "), " were removed from the rB3 obj"))
    
    return(rB3in) # end program
    
  } 
  
  if (!is.null(loc)){
    
    if (!is.numeric(loc)){
      stop("loc has to be a single number")
    }
    
    loc <- loc + 1 # consider DateTime column
    
    ######## check vars ########
    # identify the elements in the array, to be modified
    outs.idElToModify2 <- idElToModify(
      rB3in, 
      startDate = startDate,
      endDate = endDate, 
      varNames = varNames
    )
    
    # decompose the list
    colLocs2 <- outs.idElToModify2[[2]]
    colLocsNums2 <- which(colLocs2)
    
    ######## end check vars ########
    
    diffs <- setdiff(1:ncol(rB3in[["srcDF"]]),c(colLocsNums2))
    myOrder <- c(diffs[ which(diffs <= loc) ], colLocsNums2, diffs[ which(diffs > loc) ])
    
    # move vars
    rB3in[["srcDF"]] <- rB3in[["srcDF"]][,myOrder]
    rB3in[["qcDF"]] <- rB3in[["qcDF"]][,myOrder]
    rB3in[["logDF"]] <- rB3in[["logDF"]][,myOrder]
    rB3in[["ctrls"]] <- rB3in[["ctrls"]][myOrder[2:ncol(rB3in[["srcDF"]])]-1,]
    
  }
  
  return(rB3in)
  ######## ######## ########
  ######## end function ########
  ######## ######## ########
}

