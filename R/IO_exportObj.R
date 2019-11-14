#' Write the dataframes of an rB3 object (list of dataframes) to separate csv files
#'
#' Use args to select which dfs to export
#'
#' @export
#' @param rB3in data frame to be standardized
#' @param startDate start date
#' @param endDate endDate
#' @param src write source data file
#' @param qc write qc data file
#' @param metadata write log and metadata files
#' @param filePath folder to which files will be writen, set relative to working dir
#' @keywords fileIO
#' @examples rB3export(rB3in, varNames = 'All', src = TRUE, qc = TRUE, metadata = TRUE)



rB3export <- function(rB3in, startDate, endDate, varNames, src, qc, metadata, filePath) {

  ######## defaults ########
  if (missing(startDate)){
    startDate <- rB3in[["qcDF"]]$DateTime[1]
  }

  if (missing(endDate)){
    endDate <- rB3in[["qcDF"]]$DateTime[length(rB3in[["qcDF"]]$DateTime)]
  }

  if (missing(varNames)){
    varNames <- 'All'
  }

    if (missing(src)){
    src <- FALSE
  }

  if (missing(qc)){
    qc <- TRUE
  }


  if (missing(metadata)){
    metadata <- FALSE
  }

  if (missing(filePath)){
    filePath <- ""
  }


  ######## find data range for .wtr file #######
  outs.idElToModify <- idElToModify(rB3in, startDate, endDate, varNames)

  # decompose the list
  colLocs <- outs.idElToModify[[2]]
  colLocs[1] <- TRUE # make sure datetime is preserved
  colLocsNums <- which(colLocs)
  rowLocs <- outs.idElToModify[[1]]
  rowLocsNums <- which(rowLocs)

  # cull the unwanted data

  outs.idElToModify <- idElToModify(rB3in, startDate, endDate, varNames)

  # decompose the list
  colLocs <- outs.idElToModify[[2]]
  colLocs[1] <- TRUE # make sure datetime is preserved

  # cull the unwanted variables
  rB3out <- rB3in

  DFs <- c("srcDF","qcDF","logDF")

  for (f in DFs) {
    # out rows
    rB3out[[f]] <- rB3out[[f]][rB3out[[f]]$DateTime >= as.POSIXct(startDate) & rB3out[[f]]$DateTime <= as.POSIXct(endDate),]

    # out cols
    rB3out[[f]] <- rB3out[[f]][,which(colLocs)]
  }
  # cut the ctrls variables to match the new DF cols
  rB3out[["ctrls"]] <- rB3out[["ctrls"]][colLocs,]


  # write the files
rB3Name <- paste0(filePath,
                   rB3out[["metaD"]]$siteName,'_',
                   format(strptime(as.Date(startDate),"%Y-%m-%d"),"%Y%m"),'-',
                   format(strptime(as.Date(endDate),"%Y-%m-%d"),"%Y%m"),
                   '_rB3_')


  if(src == TRUE) {
    write.csv(rB3out[["srcDF"]], paste0(rB3Name,'src.csv'), row.names = FALSE, quote = FALSE)
                }

    if(qc == TRUE) {
      write.csv(rB3out[["srcDF"]], paste0(rB3Name,'qc.csv'), row.names = FALSE, quote = FALSE)
                 }

    if (metadata == TRUE) {
      write.csv(rB3out[["logDF"]], paste0(rB3Name,'logID.csv'), row.names = FALSE, quote = FALSE)
      write.csv(rB3out[["logKey"]], paste0(rB3Name,'logKey.csv'), row.names = FALSE, quote = FALSE)
      write.csv(rB3out[["ctrls"]], paste0(rB3Name,'controls-filters.csv'), row.names = TRUE, quote = FALSE)
      write.csv(rB3out[["metaD"]], paste0(rB3Name,'sitedata.csv'), row.names = FALSE, quote = FALSE)

    }

  }
