mkLongOutDF <- function(DF_in_long,DF_out, colsAffected, rowsAffected){
  ###### library ######
  
  tryCatch({
    library(tidyverse)
  },  error = function(e) {
    ans <- menu(c("Yes", "No"), title="Do you want to install tidyverse package?")
    ifelse(ans == 1,install.packages("tidyverse"),stop("Stopped because some packages are unavailable"))
  })
  
  ###### function ######
  
  # for functions that affects block of contents (i.e. rowsAffected is a vector, not matrix)
  DF_out$rowsAffected <- rowsAffected
  DF_out_long <- gather(DF_out, var, value, 2:(ncol(DF_out)-1))
  DF_out_long$colsAffected <- DF_out_long$var %in% colnames(DF_out[,2:(ncol(DF_out)-1)])
  
  browser()
  DF_long <- merge.data.frame(DF_in_long,DF_out_long, all.x = T)
  # https://stackoverflow.com/questions/11146967/efficient-alternatives-to-merge-for-larger-data-frames-r
} # end function