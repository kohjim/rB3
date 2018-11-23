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

  #   autoID <- 1
  #   if ( length(logKey$logID)  != 0 ) {
  #   autoID <- as.numeric( max(logKey$logID) + 1 )
  #   }
  #
  # # set logID (checking for numeric else set to NA)
  # if (logID == 'auto') {
  # logID <- autoID
  # } else if (is.numeric(logID) ) {
  #   logID <- logID
  # } else { logID <- NA}

  # write to logKey if valid logID is available
  logEntry <- c(logID, funName, Reason)

    logKey <-  rbind(logKey,  logEntry)
       names(logKey) <- c("logID","Function","Reason")

    # force to character
    # logKey$logID <- as.character(logKey$logID)

    # compact into unique log enries
    logKey <- logKey[!duplicated(logKey),]

    rB3in[["logKey"]] <- logKey

  }
