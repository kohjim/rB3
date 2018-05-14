#**********************************************************************************************************#
## EXAMPLE rB3 script, demonstrating various functions  ##
#@@@@ Chris McBride and Kohji Muraoka, May 2018, correspondance to cmcbride@waikato.ac.nz  @@@@@#

##set local timezone to UTC; this avoids timezone issues with POSIX date class
Sys.setenv(TZ = "UTC")

##set the root of the working directory where you will store your scripts, data and figures
setwd("C:/Users/km-admin/Dropbox/Git/rB3/wd")

## import any custom functions from customFun folder
lapply(paste0("customFun/",
              list.files(path = "customFun", pattern = "[.]R$", recursive = TRUE)),
              source)

## load any necessary libraries
library(devtools)
install("../../rB3")
library(rB3)
library(tidyverse)
#library(lubridate)
#library(httpuv)
#runExample("01_hello")

## load your metadata/site details, to be used later
metaD <- list(lakeName = "Tutira",lat = -39.227760, lon = 176.893728, timestep = 15)

# import your starting data file
myDF <- bobs2df("Tutira_200901_201801_RAW.bobs",metaD, useHydroYear = TRUE)
newDF <- include_dates(myDF,metaD,  startDate = "2009-01-09", endDate = "2009-01-10")
newDF <- include_vars(myDF,metaD,  varNames = "Tmp")
newDF <- filter_stall(newDF, metaD, cndFile = "Tutira_vars.cnd")
newDF <- filter_stall(newDF, metaD, cndFile = "Tutira_vars.cnd", endDate = "2009-01-09 16:30:0")

#**********************************************************************************************************#
###### INVESTIGATE YOUR DATA INTERACTIVELY USING SHINY ###### 
shinySetOut <- shinySet(myDF, varNames = "TmpWtr_d00050", startDate = "2008/7/1", endDate = "2018/6/30")
shiny::shinyApp(ui = shinySetOut[[1]], server = shinySetOut[[2]])

shinySetOut <- shinySet(myDF, colNum = 15)
shiny::shinyApp(ui = shinySetOut[[1]], server = shinySetOut[[2]])

shinySetOut <- shinySet_nogg(myDF, colNum = 15, startDate = "2010/1/1")
shiny::shinyApp(ui = shinySetOut[[1]], server = shinySetOut[[2]])


#**********************************************************************************************************#
##### SPECIFY ANY CUTS TO DIMENSIONS OF THE DATASET ########

##select a date range to include and remove all other dates
#newDF <- include_dates(myDF,metaData,startDate = "2008-01-01 00:00:00")

##select variables to retain and remove all others
#newDF <- include_vars(myDF, metaData, varNames = c("tmp","do","wnd","pH"))

##select variables to exclude and retain all others
#newDF <- exclude_vars(myDF, metaData, varNames = c("pH","wndDir"))

#**********************************************************************************************************# 
###### START THE QC PROCESS  ########


##filter outliers, either using a vars file, or by specifying values
newDF <- filter_max(myDF, metaD,cndFile = "Tutira_vars.cnd")
newDF <- filter_min(newDF, metaD,cndFile = "Tutira_vars.cnd")
#DFwithLog <- filter_max(DFwithLog, metaD, maxVal = 18, varNames = "Tmp", startDate = "2008-01-01 00:00:00", logID = 6)

##filter stalled data
newDF <- filter_stall(newDF, metaD, cndFile = "Tutira_vars.cnd")

##'smush' temperature data together during mixing; i.e., improve relative accuracy of temperature nodes for sensors with low accuracy
#DFwithLog <- tmp_align(newDF, metaD,varNames = c("TmpWtr"),dTPerctile = 0.2, wndSpdPerctile = 0.9)

##assign NA values to bad/invalid data (use shiny to investigate for outliers, try using lists of maintenance dates etc)

#need to add text/reasons around these deletions, i.e. log of maintenance
newDF <- assign_na(newDF, metaD, startDate = "2008-07-01 00:00:00", endDate = "2009-10-12 16:00:00", varNames = "LvlAbs")
newDF <- assign_na(newDF, metaD, startDate = "2009-09-07 12:45:00", endDate = "2009-09-07 13:30:00", varNames = "All")
newDF <- assign_na(newDF, metaD, startDate = "2010-08-01 12:45:00", endDate = "2010-09-28 11:45:00", varNames = "All")
newDF <- assign_na(newDF, metaD, startDate = "2011-01-07 11:15:00", endDate = "2011-01-11 15:45:00", varNames = "All")
newDF <- assign_na(newDF, metaD, startDate = "2011-03-09 13:00:00", endDate = "2011-03-09 13:45:00", varNames = "All")
newDF <- assign_na(newDF, metaD, startDate = "2011-03-24 15:00:00", endDate = "2011-05-06 13:45:00", varNames = "All")
newDF <- assign_na(newDF, metaD, startDate = "2011-07-14 13:45:00", endDate = "2011-07-14 14:45:00", varNames = "All")
newDF <- assign_na(newDF, metaD, startDate = "2011-09-08 10:00:00", endDate = "2011-09-08 11:00:00", varNames = "All")
newDF <- assign_na(newDF, metaD, startDate = "2012-10-12 05:45:00", endDate = "2013-05-09 09:00:00", varNames = "FlChlr")
newDF <- assign_na(newDF, metaD, startDate = "2012-11-14 10:30:00", endDate = "2012-11-14 13:30:00", varNames = "All")
newDF <- assign_na(newDF, metaD, startDate = "2013-03-07 12:00:00", endDate = "2013-03-15 11:00:00", varNames = "All")
newDF <- assign_na(newDF, metaD, startDate = "2013-04-30 10:45:00", endDate = "2013-05-16 13:30:00", varNames = "All")
newDF <- assign_na(newDF, metaD, startDate = "2015-03-30 13:30:00", endDate = "2015-03-30 14:30:00", varNames = "All")
newDF <- assign_na(newDF, metaD, startDate = "2015-06-10 12:30:00", endDate = "2015-08-03 12:45:00", varNames = "All")
newDF <- assign_na(newDF, metaD, startDate = "2015-08-16 00:45:00", endDate = "2015-08-18 08:00:00", varNames = "Tmp")
newDF <- assign_na(newDF, metaD, startDate = "2015-08-26 14:45:00", endDate = "2015-10-16 10:15:00", varNames = "Tmp","DO")
newDF <- assign_na(newDF, metaD, startDate = "2016-09-30 12:00:00", endDate = "2016-09-30 14:30:00", varNames = "All")
newDF <- assign_na(newDF, metaD, startDate = "2017-01-30 10:00:00", endDate = "2017-01-30 11:45:00", varNames = "All")

#lots of incorrect zero values in teh DO time-series, so let's set them to NA
newDF <- assign_value(newDF, metaD, oldVal = 0, newVal = NA, varNames = "DO")

#manufacturer's temperature string correction equation for innacurate calibration of early strings
newDF <- apply_eqn(newDF,startDate = "2008/07/01", endDate = "2009/06/04", varNames = "TmpWtr",c(-0.25517000,1.00566967,0.00004278,0.00000065))
newDF <- apply_eqn(newDF,startDate = "2009/06/05", endDate = "2010/07/20", varNames = "TmpWtr",c(-0.27281218,1.01046055,-0.00012528))

#make plots faceted by variable
ggplot_facetVar(DF_in = newDF, metaD = metaD, varNames = c('TmpWtr'), plotPath = 'figures/Tempr_', startDate = "2009-01-01 00:00:00", dpi = 400)
ggplot_facetVar(DF_in = newDF, metaD = metaD, varNames = c('TmpWtr_d00050','TmpWtr_d03500','Fl','Tur','Lvl','DO'), startDate = "2009-01-01 00:00:00", endDate = "2018-01-30 23:59:59", plotPath = 'figures/WQ_', dpi = 400)
ggplot_facetVar(DF_in = newDF, metaD = metaD, varNames = c('TmpAir','HumRel','PrBaro','WndSpd','WndDir','PpRain'), endDate = "2015-06-30 23:59:59", plotPath = 'figures/Met_', dpi = 400)

##make a plot of all water temperature, by year
ggplot_facetHydYr(DF_in = newDF, metaD = metaD, varNames = c('TmpWtr'), plotPath = 'figures/TmpWtr-')
ggplot_facetHydYr(DF_in = newDF, metaD = metaD, varNames = c('Fl','Turb'), plotPath = 'figures/C3-')

ggplot_single(DF_in = newDF, metaD = metaD, varNames = c('TmpWtr'), plotPath = 'figures/gg-')

## drift correction
#newDF <- fix_drift(DF_in = newDF, startDate = "2009/1/1", endDate = "2010/7/1", colNum = 12, offsets = c(0,5), span = c(30,200))

##make difference plots showing removed and retained data
# plotDiff(DF_pre = myDF, DF_post = DFwithLog,isScatter = TRUE, plotPath = 'figures/filtered')
 
##ggplot of one var
ggplot(newDF, aes(x = DateTime, y = TmpWtr_d04000)) +
   geom_line()


## re-calculate DO(mg/L) from DO (%sat) and water temperature using USGS method (custom function)
newDF <- DOsat2DOmg_ZebraTechDOpto(DF_in = newDF,DOmgColName = "DOconc_d00050", DOsatColName = "DOpsat_d00050", TColName = "TmpDOs_d00050")

## fill gaps (max rep specifies the maximum number of consecutive missing timestamps to perform interpolation across)
#newDF <- interp_na(DF_in = myDF, metaD, maxRep = 5)

##convert DF with log back to just DF
#test <- DFwithLog[[1]]

##write data to .csv
lazyWriter(DF_in = newDF, fileName = "Tutira_intermediate_rB3.bobs")

