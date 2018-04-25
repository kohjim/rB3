plotBeforeAfter <- function(DF_in,taskInf,DF_out,colsAffected,rowsAffected){
  require(ggplot2)
  require(reshape2)
  
  dir.create("../figures/debug")
  
  custom_dpi <- 150
  # browser()
  colNumbers <- which(colsAffected)
  
  if (length(colNumbers) > 0){
  
  # browser()
  for (i in 1:length(colNumbers)){
    thisColLoc <- colNumbers[i]
    thisColName <- colnames(DF_in[i])
    
    DF_plot <- DF_in[,c(1,i)]
    DF_plot$gp <- "in"
    
    tryCatch({
      ColLoc_out <- which(colnames(DF_out)==thisColName)
      thisDF_out <- DF_out[,c(1,ColLoc_out)]
      thisDF_out$gp <- "out"
      
      DF_plot <- rbind(DF_plot,thisDF_out)
      
    }, error = function(e) {
      
    })

    f1 <- ggplot(DF_plot, aes(x = DF_plot[,1],y = DF_plot[,2]))
    f1  + geom_line(aes(color = gp),size = 0.1)
        
    ggsave(
      paste0("../figures/debug/Task",taskInf[1],"_",taskInf$TaskName,"_",i,"_",thisColName,".png"),
      dpi = custom_dpi
    )
  }
  }
  
}