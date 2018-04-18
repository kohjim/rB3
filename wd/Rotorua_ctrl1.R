
# import custom functions from customFun folder
#     DOsat2DOmg_ZebraTechDOpto.R
#     [WRC_plot.R]?
setwd("C:/Users/km-admin/Dropbox/Git/rB3/wd/customFun")
lapply(list.files(pattern = "[.]R$", recursive = TRUE), source)

setwd("C:/Users/km-admin/Dropbox/Git/rB3/wd")

library(devtools)

install("../../rB3")
library(rB3)

# Metadata
metaD <- list(lakeName = "Rotorua",lat = -38.4, lon = 176, timestep = 15)

# import data file
myDF <- bobs2df("Rotorua_200707-201712_RAW_R.bobs",metaD, useHydroYear = TRUE)

###### visual assistance ###### 
shinySetOut <- shinySet(myDF, varNames = "02050", startDate = "2008/1/1", endDate = "2009/1/1")
shiny::shinyApp(ui = shinySetOut[[1]], server = shinySetOut[[2]])

shinySetOut <- shinySet(myDF, colNum = 15)
shiny::shinyApp(ui = shinySetOut[[1]], server = shinySetOut[[2]])

shinySetOut <- shinySet_nogg(myDF, colNum = 15, startDate = "2010/1/1")
shiny::shinyApp(ui = shinySetOut[[1]], server = shinySetOut[[2]])
###### visual assistance ###### 

# data wrangling
newDF <- include_dates(myDF,metaData,startDate = "2008-01-01 00:00:00")
newDF <- include_vars(myDF, metaData, varNames = c("tmp","do","wnd","pH"))
newDF <- exclude_vars(myDF, metaData, varNames = c("pH","wndDir"))

DFwithLog <- assign_na(myDF, metaD, startDate = "2007-01-01 00:00:00", endDate = "2009-07-01 00:00:00", varNames = "Tmp", logID = 1)
DFwithLog <- assign_na(DFwithLog, metaD, startDate = "2007-01-01 00:00:00", endDate = "2007-02-02 00:00:00", varNames = "Tmp", logID = 2)
DFwithLog <- assign_na(DFwithLog, metaD, startDate = "2007-01-01 00:00:00", endDate = "2009-07-01 00:00:00", varNames = "Tmp", logID = 3)

# 
# # QAQC tasks
DFwithLog <- tmp_align(DFwithLog, metaD,varNames = c("TmpWtr","TmpDOs"),dTPerctile = 0.2, wndSpdPerctile = 0.9, logID = 4)
DFwithLog <- filter_max(DFwithLog, metaD,cndFile = "Rotorua_vars.cnd", logID = 5)
DFwithLog <- filter_max(DFwithLog, metaD, maxVal = 18, varNames = "Tmp", startDate = "2008-01-01 00:00:00", logID = 6)

lazyWriter(DFwithLog, fileName = "Rotorua_intermediate.bobs", logName = "Rotorua_log.RData")

DFwithLog <- filter_min(myDF, metaD, cndFile = "Rotorua_vars.cnd", logID = 7)
DFwithLog <- filter_min(DFwithLog, metaD, minVal = 12, varNames = "Tmp", startDate = "2008-01-01 00:00:00", logID = 8)
DFwithLog <- filter_stall(DFwithLog, metaD, cndFile = "Rotorua_vars.cnd", logID = 9)

## with plot diff
DFwithLog <- filter_min(myDF, metaD, cndFile = "Rotorua_vars.cnd", logID = 7, plotPath = "filter_min")
plotDiff(myDF, DFwithLog[[1]], plotPath = "allChanges", custom_dpi = 150)
##


## DO conv
newDF <- DOsat2DOmg_ZebraTechDOpto(DF_in = myDF,DOmgColName = "DOconc_d00050", DOsatColName = "DOpsat_d00050", TColName = "TmpDOs_d00050")


## fuill gaps
newDF <- interp_na(DF_in = myDF, metaD, maxRep = 5)


lazyWriter(DF_in = myDF, fileName = "Rotorua_intermediate.bobs", startDate = "2010/1/1", endDate = "2010/7/1", varNames = c("tmp","DO"))
