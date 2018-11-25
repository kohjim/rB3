setwd("C:/Users/km-admin/Dropbox/Git/rB3/wd")

# install.packages("remotes")
# library(remotes)
# remotes::install_github("kohjim/rB3", ref = "G20", upgrade = c("never"))
# library(rB3)

# library(remotes)
# remotes::install_local(path = "../../rB3")

library(roxygen2)
setwd("C:/Users/km-admin/Dropbox/Git")
document("rB3")

setwd("C:/Users/km-admin/Dropbox/Git/rB3/wd")
library(devtools)
install("../../rB3")
library(rB3)

## read file
rB3demo <- csv2rB3("rB3demo_201507-201806_RAW_R.csv","Lake_Rotoehu",-38.5, 176.5,"NZ",TRUE)

rB3agg <- rB3stdze(rB3in = rB3demo, varNames = 'All', startDate = '2016-07-01', endDate = '2018-06-30 23:45:00',
                   timestep = 15)

rB3agg2 <- rB3agg

##
rB3agg2 <- assignVal(rB3agg2, varNames = c('TmpWtr.d00050','TmpWtr.d00150'),  
                     startDate = "2017-06-07 18:19:04", endDate = "2017-07-05 15:20:01", 
                     minVal = 12.3, maxVal = 24.9, 
                     newVal = NA, logID = "Shiny", Reason = "Manual removal", showPlot = T)


rB3agg2 <- interp_na(rB3agg2, varNames = c('TmpWtr.d00050'),  
                     maxRep = 100000, 
                     logID = "Test", Reason = "Interp", showPlot = T)

rB3agg2 <- tmp_align(rB3agg2,  
                     dTPerctile = 0.2, 
                     logID = "TestAlign", Reason = "Interp", showPlot = T)


##
install("../../rB3")
library(rB3)
shinyrB3(rB3agg2, isPlotSrc = TRUE)

shinyVar(rB3agg2, varNames = "TmpWtr.d00050", srcColour = 'red')
