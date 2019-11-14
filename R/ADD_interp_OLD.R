#' Interpolate NA values
#'
#' Assign linearly interpolated numbers for NA for a gap of up to a specified time length
#'
#' @export
#' @param dt.in a data table with
#' @param maxGap maximum length of time to interpolate over (in hours)
#' @keywords QA/QC interpolation
#' @examples my.dt <- ADD_interp(my.dt, maxgap = 60)  # interpolate all water temperatures for any gaps < 60 mins
#'

ADD_interp_OLD <- function(dt.in, maxGap){

  ######## DEFAULTS ########

  if (missing(maxGap)){
    maxGap <- 100000000000000000000
  }

  ######## END DEFAULTS ########


  # ######## find elements to modify ########
  #
  # # find vars & dates to search
  # el_toMod <- IDel_toMod(
  #   dt.in,
  #   dateRange = dateRange,
  #   varNames = varNames)
  #
  # # decompose the list
  # numsRows <- unlist(el_toMod[1], use.names = F)
  # numsCols <- unlist(el_toMod[2], use.names = F)
  #
  # ######## end find elements to modify ########


  ######## MAIN FUNCTION ########

  maxGap <- maxGap * 3600   # maxGap in seconds (for POSIX)

  df.out <- data.frame(dt.in)

  for (i in 1:length(numsCols)){ #numsCols

    thisDF <- data.frame(df.out[numsRows, c(1,numsCols[i]) ])

    if (sum(is.na(thisDF[,2])) == 0) { # if no NA then skip this iteration
      next
    }

  # find the set of rows with NAs
  isVal <- !is.na(thisDF[,2])

  ### find the start and finish of each chunk of NAs

  #stops
  bb <- isVal
  cc <- cumsum(c(bb))                                     # cumsum TRUE (i.e. non NA)
  dd <- ave(seq_along(bb), cc, FUN=seq_along)             # count continuous NAs
  naStops <- which(dd[-length(dd)] - dd[-1L] > 0) + 1     # row immediately after where NA sequence finishes

  # starts
  flip.bb <- bb[c(length(bb):1)]
  flip.cc <- cumsum(c(flip.bb))
  flip.dd <- ave(seq_along(flip.bb), flip.cc, FUN=seq_along)
  flip.ee <- length(bb) - which(flip.dd[-length(flip.dd)] - flip.dd[-1L] > 0)
  naStarts <- flip.ee[c(length(flip.ee):1)] # sort in order

  ### deal with format and end NAs
  if (is.na(thisDF[nrow(thisDF),2])){
    naStops <- c(naStops,nrow(thisDF))
    # .. and add the last ocurring value to draw a straight line
    thisDF[nrow(thisDF),2] <- thisDF[naStarts[length(naStarts)],2]
  }

  if (is.na(thisDF[1,1])){
    naStarts <- c(1, naStarts)
    # .. and add the first ocurring value to draw a straight line
    thisDF[1,2] <- thisDF[naStops[1],2]
  }

  # gaps <- data.frame(naStarts, naStops)
  spans <- as.numeric(df.out[naStops,"DateTime"]) - as.numeric(df.out[naStarts,"DateTime"])


  for (l in 1:nrow(thisDF)) {  # row by row

      # find the present timeStamp
       thisTime <- thisDF[l,"DateTime"]

       # force stopPos for times before first NA
       if (l >= naStarts[1]) {

         startPos <- naStarts[findInterval(l, naStarts)]

       } else {  startPos <- naStarts[1] }

       startTime <- thisDF[startPos, "DateTime"]

       # force stopPos for times before first NA
       if (l >= naStarts[1]) {

         stopPos <- naStops[findInterval(l, naStarts)]

       } else {  stopPos <- naStarts[1] }

       # stopPos <- naStops[findInterval(l, naStarts)]
       stopTime <- thisDF[stopPos, "DateTime"]

    span <-  as.numeric(stopTime) - as.numeric(startTime)
    spanOK <- span <= maxGap

    # browser()

    # check this row falls within the most recent NA window and that the gap length <= maxGap
    if (l >= naStarts[1] & l <= stopPos & spanOK == TRUE) {

      ## if so then interpolate
      valStart <- thisDF[startPos,2]
      valEnd <- thisDF[stopPos,2]
      valSpan <- valEnd - valStart

      thisSpan <- as.numeric(thisTime) - as.numeric(startTime)

      valNew <- valStart + valSpan * (thisSpan / span)

      # add the new interpolated value
      thisDF[l,2] <- valNew

      } # finish interp

    } # finish row-by-row for this col

  # stitch the modified data back in
  df.out[numsRows, numsCols[i]] <- thisDF[,2]

  } # finish looping through cols

  dt.out <- data.table(df.out)

  # return the modified table
  return(dt.out)

  ######## end function ########
}
