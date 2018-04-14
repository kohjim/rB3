readCsvToDF <- function(filePath, timestep, isHydroYear) {
  
  # isHydroYear: 1: yes, 2: no
  
  ######## packa checks - need to delete unused paclage
  
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
    library(lubridate)
  },  error = function(e) {
    ans <- menu(c("Yes", "No"), title="Do you want to install lubridate package?")
    ifelse(ans == 1,install.packages("lubridate"),stop("Stopped because some packages are unavailable"))
  })
  
  ######## read file
  
  DF_in <-
    read.csv(filePath, header = TRUE)
  
  DF_in$DateTime <-
    as.POSIXct(DF_in$DateTime,
               origin = "1970-01-01 00:00:00",
               format = "%Y-%m-%d %H:%M:%S",
               tz = "Etc/GMT+12")
  
  
  ######## expand dataframe to full hydrological years
  if (isHydroYear){
    YrStart <-
      ifelse(month(min(DF_in$DateTime)) < 7, year(min(DF_in$DateTime)) - 1, year(min(DF_in$DateTime)))
    
    YrEnd   <-
      ifelse(month(max(DF_in$DateTime)) < 7, year(max(DF_in$DateTime)), year(max(DF_in$DateTime)) +
               1)
    
    dates   <-
      data.frame(seq.POSIXt(
        as.POSIXct(
          paste0(YrStart, "-07-01 00:00:00"),
          origin = "1970-01-01 00:00:00",
          tz = "Etc/GMT+12"
        ),
        as.POSIXct(
          paste0(YrEnd, "-06-30 23:45:00"),
          origin = "1970-01-01 00:00:00",
          tz = "Etc/GMT+12"
        ),
        timestep * 60
      ))
    
    names(dates) <- "DateTime"
    
    DF_in <- merge(DF_in, dates, all.y = T)
  }
  
} # end function