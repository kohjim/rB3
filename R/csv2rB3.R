#' Convert a csv file of time-series observations into an rB3 object (list of dataframes)
#'
#' Imports a csv file to rB3
#'
#' Creates a list of dataframes, including: \cr
#'      1. 'srcDF'  ; the unmodified data \cr
#'      2. 'qcDF'   ; a copy of the data, which will be modified by rB3\cr
#'      3. 'logDF'  ; a 'log' file with similar structure to srcDF and qcDF, to store QC operations log\cr
#'      4. 'logKey' ; control key with logIDs and explanatory columns
#'      5. 'ctrls'  ; controls, extracted from extra header rows in the raw csv files ('ctrls')\cr
#'      6. 'metaD'  ; site metadata, create from additional input args ('metaD')\cr
#'
#' Import control variables by matching header rows (first col) to the following tags:
#'
#' FIRST COLUMN :   FORMAT  : COLUMNS 2:ncol ...
#'
#' "measVar"    : character : measurement variable represented by the column\cr
#' "units"      : character : units of measurement\cr
#' "sensorDist" : numeric   : vertical position of the sensor (e.g., below water surface, above ground, etc)\cr
#' "plotLabels" : character : label that will appear on rB3 plot panels\cr
#' "methodAgg"  : character : method of aggregation used for rB3stdze function\cr
#' "pullAgg"    : character : direction of aggregation for rB3stdze function ('left', 'centre' or 'right')\cr
#' "filterMin"  : numeric   : minimum allowable value\cr
#' "filterMax"  : numeric   : maximum allowable value\cr
#' "filterRoc"  : numeric   : maximum allowable rate of change\cr
#' "filterReps" : numeric   : maximum allowable repeated consecutive values\cr
#' "filterMean" : numeric   : time window (in hours) for running mean w/ standrard deviation filter\cr
#' "filterSD"   : numeric   : number of standard deviations for cutoff\cr
#'
#'
#' @export
#' @param filePath file path of source csv file, timestamps in first column as yyyy-mm-dd hh:mm:ss with header "DateTime"
#' @param siteName name of the monitoring station (character)
#' @param lat latitude of the monitoring station (numeric, decimal WGS84 degrees)
#' @param lon longitude of the monitoring station (numeric, decimal WGS84 degrees)
#' @param country country of origin (character)
#' @keywords fileIO
#' @examples csv2rB3("Rotorua.csv", siteName = "Lake_Rotorua", lat = -38, long = 176, country = "NZ")


csv2rB3 <- function(filePath, siteName, lat, lon, country) {


  srcData <- data.table::fread(filePath)

  # find header row
  hRow <- which(srcData[,1] == "DateTime")

  # find rows that begin with a POSIX date to include in the measurement data frames
  srcDF <- srcData[hRow:nrow(srcData),]
       colnames(srcDF) <- as.character(unlist(srcDF[1,]))
       srcDF <- srcDF[-1,]


  # define date format
  srcDF$DateTime <-  as.POSIXct(srcDF$DateTime,
                       origin = "1970-01-01 00:00:00",
                       format = "%Y-%m-%d %H:%M:%S",
                       tz = "UTC")

  # remove all non-numeric characters
  srcDF[2:ncol(srcDF)] <- apply(srcDF[2:ncol(srcDF)], 2, function(y) as.numeric(gsub("[^0-9.-]", "", y)))

  # sort into chronological order, just in case
  srcDF <- srcDF[order(srcDF$DateTime),]

  # make copy for applying QC to
  qcDF <- srcDF

  # make copy for log entries
  logDF <- srcDF
  logDF[1:nrow(logDF),2:ncol(logDF)] <- NA

  # make dataframe of controls

  ctrls <- data.frame(matrix(NA, nrow = 0, ncol = ncol(srcData) -1))
  colnames(ctrls) <- srcData[hRow,2:ncol(srcData)]

  ctrlnames <- c("measVar","units","sensorDist","sensorLoc","plotLabels","methodAgg","pullAgg",
                 "maxVal","minVal","maxRoc","maxReps","filterWindow","filterSD")

for (n in ctrlnames) {

  ctrls[n,] <- rep(NA,ncol(ctrls))
  try( ctrls[n,] <- srcData[srcData[,1] == n,2:ncol(srcData)] , silent=TRUE ) }

  ctrls <- data.frame( t(ctrls) )

  chrCols <- c("measVar", "units", "sensorLoc","plotLabels","methodAgg","pullAgg")

  for (z in chrCols) {
    ctrls[,z] <- as.character(ctrls[,z])
  }

  numCols <- c("sensorDist","minVal","maxVal","maxRoc","maxReps","filterWindow","filterSD")

  for (y in numCols) {
    ctrls[,y] <- as.numeric(as.character(ctrls[,y]))
  }


  # make metadata

  metaD <- list(siteName = siteName, lat = lat, lon = lon, country = country)

  # make log Key

  logKey <- data.frame(matrix(NA, nrow = 0, ncol = 3) )
   names(logKey) <- c("logID","Function","Reason")

  ## combine into rB3object
  rB3object <- list(srcDF,qcDF,logDF,logKey, ctrls,metaD)
  names(rB3object) <- c("srcDF","qcDF","logDF","logKey","ctrls","metaD")

  return(rB3object)

}
