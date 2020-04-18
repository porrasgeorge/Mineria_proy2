
get_SourceIDbyName <- function(sources, selName){
  ID <- sources %>% filter(Name == selName)
  sourceID<- ID[1,1]
  return(sourceID)
}

create_Percent_Table <- function(dataLog_table, tensiones){
  dataLog_table$Perc <- label_percent()(dataLog_table$Perc)
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
