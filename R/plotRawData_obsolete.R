plotRawData <- function(site, fileName, timestep) {
  custom_dpi <- 150
  isPlotAllVarsSeparately <- 1
  isPlotSomeVarsTogether <- 1
  
  ######## import functions
  source("readCsvToDF.R")
  
  ######## packa checks
  
  tryCatch({
    library(reshape2)
  },  error = function(e) {
    ans <- menu(c("Yes", "No"), title="Do you want to install reshape2 package?")
    ifelse(ans == 1,install.packages("reshape2"),stop("Stopped because some packages are unavailable"))
  })
  
  tryCatch({
    library(bindr)
  },  error = function(e) {
    ans <- menu(c("Yes", "No"), title="Do you want to install bindr package?")
    ifelse(ans == 1,install.packages("bindr"),stop("Stopped because some packages are unavailable"))
  })
  
  tryCatch({
    library(tidyverse)
  },  error = function(e) {
    ans <- menu(c("Yes", "No"), title="Do you want to install tidyverse package?")
    ifelse(ans == 1,install.packages("tidyverse"),stop("Stopped because some packages are unavailable"))
  })
  
  tryCatch({
    library(dplyr)
  },  error = function(e) {
    ans <- menu(c("Yes", "No"), title="Do you want to install dplyr package?")
    ifelse(ans == 1,install.packages("dplyr"),stop("Stopped because some packages are unavailable"))
  })
  
  tryCatch({
    library(ggplot2)
  },  error = function(e) {
    ans <- menu(c("Yes", "No"), title="Do you want to install ggplot2 package?")
    ifelse(ans == 1,install.packages("ggplot2"),stop("Stopped because some packages are unavailable"))
  })
  
  tryCatch({
    library(lubridate)
  },  error = function(e) {
    ans <- menu(c("Yes", "No"), title="Do you want to install lubridate package?")
    ifelse(ans == 1,install.packages("lubridate"),stop("Stopped because some packages are unavailable"))
  })
  
  ######## add figure paths
  tryCatch({
    dir.create(paste0("../figures"))
  },  error = function(e){
  })
  
  tryCatch({
    dir.create(paste0("../figures/", site))
  },  error = function(e){
  })
  
  ######## read raw data
  DF_in <- readCsvToDF(paste0("../data/", site, "/", fileName[1]),timestep,1)
  
  #add some more date categories to facet the plots by
  DF_in$month <- month(DF_in$DateTime)
  
  DF_in$hydroyr <- ifelse(DF_in$month < 7,
                          year(DF_in$DateTime),
                          year(DF_in$DateTime) + 1)
  
  
  ######## read QC control file 
  
  ctrl <- read.csv(paste0("../data/", site, "/",  site, "_ctrl.csv"), header = FALSE)
  labs <- head(ctrl, 2)
  
  ### MAKE RAW DATA PLOTS ###
  
  ## plot all vars one-by-one
  if (isPlotAllVarsSeparately){
    for (i in 2:(ncol(DF_in) - 2)) {
      ggplot(DF_in, aes(x = DateTime, y = DF_in[, i])) +
        geom_line(size = 0.01) +
        ylab(labs[2, i]) +
        xlab("Date") +
        facet_wrap(~ hydroyr, ncol = 1, scales = 'free_x')
      
      ggsave(
        paste0(
          "../figures/",
          site,
          "/RAW_",
          labs[1, i],
          ".png"
        ),
        width = 8,
        height = nrow(DF_in) / 10000,
        dpi = custom_dpi
      )
    }
  }
  
  ##plot groups, e.g. water temperature, DO
  if (isPlotSomeVarsTogether){
    
    #longformat
    dfl <- gather(DF_in, var, value, 2:(ncol(DF_in) - 2))
    
    #temperature
    labelsToPlot <- c("TmpWtr", "TmpDOs")
    tmpwtr <- dfl[grepl(paste(labelsToPlot, collapse = "|"), dfl$var),]
    
    ggplot(tmpwtr, aes(x = DateTime, y = value)) +
      geom_line(aes(color = var), size = 0.005) +
      ylab("Temperature (degC)") +
      xlab("Date") +
      facet_wrap(~ hydroyr, ncol = 1, scales = 'free_x')
    
    ggsave(
      paste0("../figures/", site, "/", 'RAW_TmpWtr_HydroYr.png'),
      width = 8,
      height = nrow(tmpwtr) / 250000,
      dpi = custom_dpi
    )
    
    ggplot(tmpwtr, aes(x = DateTime, y = value)) +
      geom_line(aes(color = var), size = 0.005) +
      ylab("Temperature (degC)") +
      xlab("Date")
    
    ggsave(
      paste0("../figures/", site, "/", 'RAW_TmpWtr.png'),
      width = 10,
      height = 4,
      dpi = custom_dpi
    )
    
    #dissolved oxygen saturation
    dopsat <- dfl[grepl("DOpsat", dfl$var),]
    
    ggplot(dopsat, aes(x = DateTime, y = value)) +
      geom_line(aes(color = var), size = 0.005) +
      ylab("Dissolved oxygen (%sat)") +
      xlab("Date") +
      facet_wrap(~ hydroyr, ncol = 1, scales = 'free_x')
    
    ggsave(
      paste0("../figures/", site, "/", 'RAW_DOpsat_HydroYr.png'),
      width = 8,
      height = nrow(tmpwtr) / 250000,
      dpi = custom_dpi
    )
    
    ggplot(dopsat, aes(x = DateTime, y = value)) +
      geom_line(aes(color = var), size = 0.005) +
      ylab("Dissolved oxygen (%sat)") +
      xlab("Date")
    
    ggsave(
      paste0("../figures/", site, "/", 'RAW_DOpsat.png'),
      width = 8,
      height = 4,
      dpi = custom_dpi
    )
  }
} # end function