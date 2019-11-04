library(data.table)

dt_in <- data.table::fread('rB3demo_201507-201806_RAW_R_noctrls.csv')

dt_in$DateTime <-  as.POSIXct(dt_in$DateTime,
                              origin = "1970-01-01 00:00:00",
                              format = "%Y-%m-%d %H:%M:%S",
                              tz = "UTC")

test <- aggPlotData(dt_in[,1:5])

# library(rB3)

test <- aggTS(dt_in, FUN = 'mean', pullAgg = 'centre')


test$src <- test$value / 2
test$hl <- test$value * 2


rB3plotr(test,  cols.qc = 'black', cols.src = 'pink', cols.hl = 'red', geom = 'dots', siteName = 'Test_site', facet = T, savePlot = '') # cols.qc = c('purple','red'),
