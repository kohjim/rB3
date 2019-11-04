# function to aggregate one var, using data table
agg1var <- function(dt_in, FUN, varName) {

  # trim to the varName of interest, as data table
  onevar <- dt_in[!is.na(dt_in[,get(varName)]),mget(c("DateTime",varName))]

  # find stats, bookend with means so that lines render to the means rather than extremes
  stats <- setDT(stats)[, .SD, sum, by = 'DateTime']

  setDT(f1)[, lapply(.SD, sum, na.rm = TRUE), by = .(name,t)]

  # convert to long
  stats = melt(stats, id.vars = c("DateTime"),
               measure.vars = varName)

  # # convert to data frame for ggplot
  # aggd <- as.data.frame(rbind(nasVar, stats[,c(1,3)]))
  # # add variable name for long merge
  # aggd$var <- varName
  stats <- stats[order(aggd$DateTime),]

  return(stats)
}

testagg <- agg1var(dt_in, FUN = mean, varName = names(dt_in)[2])
