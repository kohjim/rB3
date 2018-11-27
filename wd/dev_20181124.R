setwd("C:/Users/km-admin/Dropbox/Git/rB3/wd")

# install.packages("remotes")
# library(remotes)
# remotes::install_github("kohjim/rB3", ref = "G20", upgrade = c("never"))
# library(rB3)

library(remotes)
remotes::install_local(path = "../../rB3")

# install.packages("roxygen2")
library(roxygen2)
library(devtools)
setwd("C:/Users/km-admin/Dropbox/Git")
document("rB3")

setwd("C:/Users/km-admin/Dropbox/Git/rB3/wd")
library(devtools)
install("../../rB3")
library(rB3)

## read file
rB3demo <- csv2rB3("rB3demo_201507-201806_RAW_R.csv","Lake_Rotoehu",-38.5, 176.5,"NZ",TRUE)

system.time({rB3agg <- rB3stdze(rB3in = rB3demo, 
                   varNames = 'All', 
                   startDate = '2016-07-01', 
                   endDate = '2018-06-30 23:45:00',
                   timestep = 15,
                   aggAll = FALSE)})

rB3agg2 <- rB3agg

##
rB3agg2 <- assignVal(rB3agg2, varNames = 'TmpWtr',  
                     startDate = "2017-06-07 18:19:04", endDate = "2017-07-05 15:20:01", 
                     minVal = 12.3, maxVal = 24.9, 
                     newVal = NA, logID = "Shiny", Reason = "Manual removal", showPlot = T)


system.time({rB3agg2 <- interp_na(rB3agg2, varNames = c('TmpWtr.d00050'),  
                     maxRep = 100000, 
                     logID = "Test", Reason = "Interp")})

system.time({rB3agg2 <- applyInterp(rB3agg2, varNames = c('TmpWtr.d00050'))})

rB3agg2 <- tmp_align(rB3agg2,  
                     dTPerctile = 0.2, 
                     logID = "TestAlign", Reason = "Interp", showPlot = T)


##
install("../../rB3")
library(rB3)
shinyrB3(rB3agg2)

shinyVar(rB3agg2, varNames = "TmpWtr.d00050", srcColour = 'red')

rB3in <- rB3agg2

# create empty variables called DOSat1, DOSat2, DOSat3 after third column (3rd variables, excluding datetime)
rB3agg3 <- varWrangle(rB3agg2, varNames = c("DOSat1","DOSat2","DOSat3"), loc = 3)
rownames(rB3agg3[['ctrls']])

# move variables including keyword "Wnd" or "Rad" to after fourth column
rB3agg3 <- varWrangle(rB3agg3, varNames = "Wnd|Rad", task = "moveto", loc = 4)
rownames(rB3agg3[['ctrls']])

# remove variables includes keyword "TmpWtr"
rB3agg3 <- varWrangle(rB3agg3, varNames = "TmpWtr", task = "rm", loc = 3)
rownames(rB3agg3[['ctrls']])
