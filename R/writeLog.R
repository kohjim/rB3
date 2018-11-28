#' writes to the rB3 logKey dataframe
#'
#' @param rB3in rB3 object input
#' @param logID write an operation identifier to the log frames, default = NA, 'auto' chooses a log number for you, or provide a numerical code
#' @param funName name of the operation associated with the log event
#' @param funName the reason for the log event
#' @keywords logging
#' @export
#' @examples writeLog(rB3in, logID, funName, Reason)

writeLog <- function(rB3in, logID, funName, Reason) {

  ######## defaults ########

  if (missing(logID)){
    logID <- NA
  }

  if (missing(Reason)){
    Reason <- "Not specified"
  }

  if (missing(funName)){
    funName <- "Not specified"
  }

  ######## end defaults ########

  ######## function ########

  logKey <- rB3in[["logKey"]]

  # write to logKey if valid logID is available
    logKey <-  rbind(logKey,  c(logID, funName, Reason) )
       names(logKey) <- c("logID","Function","Reason")

    # force to character
    for (n in names(logKey)) {

       logKey[,n] <- as.character(logKey[,n])

    }


    # compact into unique log enries
    logKey <- logKey[!duplicated(logKey),]

    rB3in[["logKey"]] <- logKey

    return(rB3in)

  }
