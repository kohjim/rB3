#' Interpolate NA values
#'
#' Assign linearly interpolated numbers for NA for a gap of up to a specified time length
#'
#' @export
#' @param values a vector of numeric values
#' @param DateTimes a vector of POSIXct timestamps corresponding to values vector
#' @param maxGap maximum length of time to interpolate over (in hours)
#' @keywords QA/QC interpolation
#' @examples my.dt <- ADD_interp(my.dt, maxgap = 60)  # interpolate all water temperatures for any gaps < 60 mins
#'

ADD_interp <- function(values, DateTimes, maxGap){

  ######## DEFAULTS ########

  # if (missing(DateTimes)){
  #   print('Please provide a valid vector of dates')
  # }
  #
  # if (missing(values)){
  #   print('Please provide a valid vector of numeric measurement')
  # }

    if (missing(maxGap)){
    maxGap <- 100000000000000000000
  }

  ######## END DEFAULTS ########


  ######## MAIN FUNCTION ########

  maxGap <- maxGap * 3600   # maxGap in seconds (for POSIX)

  ### find the start and finish of each chunk of NAs

  #stops
  isVal <- !is.na(values)
  cs <- cumsum(c(isVal))                                  # cumsum TRUE (i.e. non NA)
  contNA <- ave(seq_along(isVal), cs, FUN=seq_along)             # count continuous NAs
  naStops <- which(contNA[-length(contNA)] - contNA[-1L] > 0) + 1     # row immediately after where NA sequence finishes

  # starts
  flip.isVal <- isVal[c(length(isVal):1)]
  flip.cs <- cumsum(c(flip.isVal))
  flip.contNA <- ave(seq_along(flip.isVal), flip.cs, FUN=seq_along)
  naStarts <- length(isVal) - which(flip.contNA[-length(flip.contNA)] - flip.contNA[-1L] > 0)
  naStarts <- naStarts[c(length(naStarts):1)] # sort in order

  ### deal with format and end NAs
  if ( is.na(values[length(values)]) ) {
    naStops <- c(naStops,length(values))
    # .. and add the last ocurring value to draw a straight line
    values[length(values)] <- values[naStarts[length(naStarts)] ]
  }

  if ( is.na(values[1]) ) {
    naStarts <- c(1, naStarts)
    # .. and add the first ocurring value to draw a straight line
    values[1] <- values[naStops[1]]
  }


  ##### do the interpolation ###

   for (l in 1:length(values)) {  # row by row

      # find the present timeStamp
       thisTime <- DateTimes[l]


       # force startPos for times before first NA
       if (l >= naStarts[1]) {

         startPos <- naStarts[findInterval(l, naStarts)]

       } else {  startPos <- naStarts[1] }

            startTime <- DateTimes[startPos]

       # force stopPos for times before first NA
       if (l >= naStarts[1]) {

         stopPos <- naStops[findInterval(l, naStarts)]

       } else {  stopPos <- naStarts[1] }

            stopTime <- DateTimes[stopPos]


    span <-  as.numeric(stopTime) - as.numeric(startTime)
    spanOK <- span <= maxGap

    # browser()

    # check this row falls within the most recent NA window and that the gap length <= maxGap
    if (l >= naStarts[1] & l <= stopPos & spanOK == TRUE) {

      ## if so then interpolate
      valStart <- values[startPos]
      valEnd <- values[stopPos]
      valSpan <- valEnd - valStart

      thisSpan <- as.numeric(thisTime) - as.numeric(startTime)

      valNew <- valStart + valSpan * (thisSpan / span)

      # add the new interpolated value
      values[l] <- valNew

      } # finish interp

    } # finish row-by-row

  # return the modified table
  return(values)

}  ######## end function ########
