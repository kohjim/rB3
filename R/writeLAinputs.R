#' Write input files for Lake Analyzer from your rB3 files
#'
#' Easily generate Lake Analyzer water temperature (.wtr) and wind speed (.wnd) files with the correct header formats
#'
#' @export
#' @param rB3in data frame to be standardized
#' @param metaD site metadata, including desired timestep (in minutes)
#' @param startDate measurements prior to this time will be removed
#' @param endDate measurements after this time will be removed
#' @param wtrNames variables (columns) of water temperature to be written to .wtr file
#' @param wtrDepths depths for water temperature cols, for headers of .wtr file
#' @param wndNames variable (column) containing wind speed data .wnd file
#' @param wndHeight height of wind sensor (in m) above water surface, used to stadardise wind speed to 10 m height using wind power law
#' @param filePath folder to which files will be writen, set relative to working dir
#' @keywords fileIO
#' @examples stdDF <- rB3stdze(df = rawDF, varNames = wqVars, metaD = metaD, startDate = '2016-07-01 00:00:00', endDate = '2018-06-30 23:45:00', doAgg = TRUE, methodAgg = ctrls$methodAgg, pullAgg = ctrls$pullAgg)
#'
#'


writeLAinputs <- function(rB3in, metaD, startDate, endDate, wtrNames, wndName, wndHeight, filePath) {

browser()
    ######## defaults ########

  if (missing(startDate)){
    startDate <- as.POSIXct( max( rB3in[["qcDF"]]$DateTime),
                             origin = "1970-01-01 00:00:00",
                             format = "%Y-%m-%d %H:%M:%S",
                             tz = "UTC" )
  }

  if (missing(endDate)){
    endDate <- as.POSIXct( max( rB3in[["qcDF"]]$DateTime),
                           origin = "1970-01-01 00:00:00",
                           format = "%Y-%m-%d %H:%M:%S",
                           tz = "UTC" )
  }

  if (missing(filePath)){
    filePath <- ""
  }

  if (missing(wndHeight)){
    wndHeight <- 10
  }

  # retrieve working dataframe
  df <- rB3in[["qcDF"]]


  ######## find data for .wtr file #######
  wtr.idElToModify <- idElToModify(df, startDate, endDate, wtrNames)

  # decompose the list
  colLocs <- wtr.idElToModify[[2]]
  colLocs[1] <- TRUE # make sure datetime is preserved
  colLocsNums <- which(colLocs)

  # cull the unwanted data
  wtr <- df[df$DateTime >= as.POSIXct(startDate) & df$DateTime <= as.POSIXct(endDate),colLocsNums]

  # find matching depths from ctrls
  hdrLocsNums <- colLocsNums - 1

  wtrHdrs <- paste0('wtr_',rB3in[["ctrls"]] [hdrLocsNums,"sensorDist"])

  colnames(wtr) <- c("DateTime",wtrHdrs)

  write.csv(wtr, paste0(filePath,metaD$lakeName,'.wtr'), row.names = FALSE, quote = FALSE)

  ######## find data for .wnd file #######
  wnd.idElToModify <- idElToModify(df, startDate, endDate, wndName)

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
