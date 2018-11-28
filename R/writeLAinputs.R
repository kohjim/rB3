#' Write input files for Lake Analyzer from your rB3 files
#'
#' Easily generate Lake Analyzer water temperature (.wtr) and wind speed (.wnd) files with the correct header formats
#'
#' @export
#' @param rB3in data frame to be standardized
#' @param startDate start date
#' @param endDate endDate
#' @param wtrNames variables (columns) of water temperature to be written to .wtr file
#' @param wndName variable (column) of wind speed data to be written to .wnd file
#' @param wndHeight height of wind speed sensor above lake surface
#' @param filePath folder to which files will be writen, set relative to working dir
#' @keywords fileIO
#' @examples writeLAinputs(rB3in = rB3agg2, wtrNames = 'TmpWtr', wndName = 'WndSpd', wndHeight = 1.5)
#'
#'


writeLAinputs <- function(rB3in, startDate, endDate, wtrNames, wndName, wndHeight, filePath) {

  ######## defaults ########
  if (missing(startDate)){
    startDate <- rB3in[["qcDF"]]$DateTime[1]
  }

  if (missing(endDate)){
    endDate <- rB3in[["qcDF"]]$DateTime[length(rB3in[["qcDF"]]$DateTime)]
  }

  if (missing(wtrNames)){
    wtrNames <- "T"
  }

  if (missing(wndName)){
    wndName <- "W"
  }


  if (missing(wndHeight)){
    wndHeight <- 10
  }

  if (missing(filePath)){
    filePath <- ""
  }

  # retrieve working dataframe
  df <- rB3in[["qcDF"]]


  ######## find data for .wtr file #######
  wtr.idElToModify <- idElToModify(rB3in, startDate, endDate, wtrNames)

  # decompose the list
  colLocs <- wtr.idElToModify[[2]]
  colLocs[1] <- TRUE # make sure datetime is preserved
  colLocsNums <- which(colLocs)

  # cull the unwanted data
  wtr <- df[df$DateTime >= as.POSIXct(startDate) & df$DateTime <= as.POSIXct(endDate),colLocsNums]

  # find matching depths from ctrls

  hdrLocsNums <- colLocsNums[2:length(colLocsNums)]
  hdrLocsNums <- hdrLocsNums - 1

  wtrHdrs <- paste0('wtr_',rB3in[["ctrls"]] [hdrLocsNums,"sensorDist"])

  colnames(wtr) <- c("DateTime",wtrHdrs)


  write.csv(wtr, paste0(filePath,rB3in[["metaD"]]$siteName,'.wtr'), row.names = FALSE, quote = FALSE)

  ######## find data for .wnd file #######
  wnd.idElToModify <- idElToModify(rB3in, startDate, endDate, wndName)

  # decompose the list
  colLocs <- wnd.idElToModify[[2]]
  colLocs[1] <- TRUE # make sure datetime is preserved
  colLocsNums <- which(colLocs)

  # cull the unwanted data
  wnd <- df[df$DateTime >= as.POSIXct(startDate) & df$DateTime <= as.POSIXct(endDate),colLocsNums]

  # find matching depths from ctrls
  colnames(wnd) <- c("DateTime","wnd")

  # apply power law to standardise wind to 10 m
  wnd$wnd <- wnd$wnd*(10/wndHeight)^0.11

  write.csv(wnd, paste0(filePath,rB3in[["metaD"]]$siteName,'.wnd'), row.names = FALSE, quote = FALSE)
}
