
get_SourceIDbyName <- function(sources, selName){
  ID <- sources %>% filter(Name == selName)
  sourceID<- ID[1,1]
  return(sourceID)
}

create_Percent_Table <- function(dataLog_table, tensiones){
  print("excecuted....")
  dataLog_table$Perc <- label_percent(accuracy = 0.01)(dataLog_table$Perc)
  dataLog_table$Freq <- as.integer(dataLog_table$Freq)
#  browser()
  t1 <- dataLog_table %>% select(Classif, Quantity, Freq) %>% spread(Quantity, value = Freq, fill = 0)
  t2 <- dataLog_table %>% select(Classif, Quantity, Perc) %>% spread(Quantity, value = Perc, fill = 0)
  t3 <- t1 %>% left_join(t2, by = "Classif")
  rm(t1, t2)
  t3$Lim_Inferior <- c(0, tensiones["limit087"], tensiones["limit091"], tensiones["limit093"], tensiones["limit095"], tensiones["limit105"], tensiones["limit107"], tensiones["limit109"], tensiones["limit113"])
  t3$Lim_Superior <- c(tensiones["limit087"], tensiones["limit091"], tensiones["limit093"], tensiones["limit095"], tensiones["limit105"], tensiones["limit107"], tensiones["limit109"], tensiones["limit113"], 100000)
  t3 <- t3[,c(1, 8, 9, 2, 5, 3 , 6, 4, 7)]
  colnames(t3) <- c("Clasificacion", "Lim_Inferior", "Lim_Superior", "Cantidad_Vab", "Porcent_Vab", "Cantidad_Vbc", "Porcent_Vbc", "Cantidad_Vca", "Porcent_Vca")  
  
  return(t3)
}

create_Histo_Plot <- function(IonData_LineV, title){
  p <- ggplot(IonData_LineV, aes(x = Classif, y = Perc, label = Perc, fill = Quantity )) +
    geom_col(position = "dodge", width = 0.7) +
    scale_y_continuous(labels = function(x) paste0(100*x, "%"), limits = c(0, 1.2)) +
    geom_text(aes(label=sprintf("%0.2f%%", 100*Perc)), 
              position = position_dodge(0.9), 
              angle = 90, size = 3) + 
    theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 8)) +
    ggtitle(title) +
    xlab("Medicion") + 
    ylab("Porcentaje")
  
  return(p)
}

create_Excel_file <- function(table, ws_name){
  wb <- createWorkbook()
  addWorksheet(wb, ws_name)
  setColWidths(wb, ws_name, cols = c(1:10), widths = c(20, rep(15, 9) ))
  writeDataTable(wb, ws_name, x = table, startRow = 1, rowNames = F, tableStyle = "TableStyleMedium1")
  return(wb)
  
  
}