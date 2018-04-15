# not run
setwd("C:/Users/km-admin/Dropbox/Git/rB3/wd")

library(devtools)

install("../../rB3")
library(rB3)

# Metadata
metaD <- list(lakeName = "Rotorua",lat = -38.4, lon = 176, timestep = 15)

# import data file
myDF <- bobs2df("Rotorua_200707-201712_RAW_R.bobs",metaD, useHydroYear = TRUE)

# data wrangling
newDF <- include_dates(myDF,metaData,startDate = "2008-01-01 00:00:00")
newDF <- include_vars(myDF, metaData, varNames = c("tmp","do","wnd","pH"))
newDF <- exclude_vars(myDF, metaData, varNames = c("pH","wndDir"))

DFwithLog <- assign_na(myDF, metaD, startDate = "2007-01-01 00:00:00", endDate = "2009-07-01 00:00:00", varNames = "Tmp", logID = 1)
DFwithLog <- assign_na(DFwithLog, metaD, startDate = "2007-01-01 00:00:00", endDate = "2007-02-02 00:00:00", varNames = "Tmp", logID = 2)
DFwithLog <- assign_na(DFwithLog, metaD, startDate = "2007-01-01 00:00:00", endDate = "2009-07-01 00:00:00", varNames = "Tmp", logID = 3)

# 
# # QAQC tasks
DFwithLog <- tmp_align(DFwithLog,metaD,varNames = c("TmpWtr","TmpDOs"),dTPerctile = 0.2, wndSpdPerctile = 0.9, logID = 4)
DFwithLog <- filter_max(DFwithLog,metaD,cndFile = "Rotorua_vars.cnd", logID = 5)
DFwithLog <- filter_max(DFwithLog,metaD, maxVal = 18, varNames = "Tmp", startDate = "2008-01-01 00:00:00", logID = 6)
# lazyWrite(DFwithLog, writeLog = TRUE)
DFwithLog <- filter_min(DFwithLog,metaD,cndFile = "Rotorua_vars.cnd", logID = 7)
DFwithLog <- filter_min(DFwithLog,metaD, minVal = 12, varNames = "Tmp", startDate = "2008-01-01 00:00:00", logID = 8)
DFwithLog <- filter_stall(DFwithLog, metaD, cndFile = "Rotorua_vars.cnd", logID = 9)
# lazyWrite(DFwithLog,metaData, writeLog = TRUE)
# 
