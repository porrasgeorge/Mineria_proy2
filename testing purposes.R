
dataLog <- dataLog %>% filter(!grepl('Sub Laguna-SD79SWinDemand|Sub Zarcero-SD79SWinDemand', Meter))
## write_feather(dataLog, "featherFiles/dataLog_v2.feather")



DB_getMeterVarCount <- function(){
  
  final_dateCR <- floor_date(now(), "day") ## corte hasta hoy
  initial_dateCR <- final_dateCR - months(1) ## 3 meses hacia atras 
  initial_date <- with_tz(initial_dateCR, tzone = "UTC") 
  final_date <- with_tz(final_dateCR, tzone = "UTC")
  
  channel <- odbcConnect("SQL_ION", uid="sa", pwd="Con3$adm.")
  sources <- sqlQuery(channel , "select top 1000 ID, Name from Source")
  quantity <- sqlQuery(channel , "select top 10000 ID, Name from Quantity")
  
  dataLog <- sqlQuery(channel , paste0("select SourceID, QuantityID, count(*) as Cantidad from dataLog2 where ",
                                       " TimestampUTC >= '", initial_date, "'",
                                       " and TimestampUTC < '", final_date, "'",
                                       " group by SourceID, QuantityID"))
  
  odbcCloseAll()
  
  d2 <- dataLog %>% left_join(quantity, by = c('QuantityID' = "ID")) %>%
    left_join(sources, by = c('SourceID' = "ID"))
  d2$SourceID <- NULL
  d2$QuantityID <- NULL
  colnames(d2) <- c("Cantidad", "Quantity", "Meter")
  d2 <- d2 %>% arrange(Meter, Quantity)
  
  
  
  
  dataLog$Name <- as.factor(dataLog$Name)
  dataLog$Cooperative <- as.factor(dataLog$Cooperative)
  dataLog$Meter <- gsub(pattern = "_", replacement = " ", dataLog$Meter)
  dataLog$Meter <- as.factor(dataLog$Meter)
  names(dataLog)[names(dataLog) == "Name"] <- "Quantity"
  dataLog$SourceID <- NULL
  dataLog$QuantityID <- NULL
  
  meters_class  <- meter_classes(dataLog$Meter)
  dataLog <- dataLog %>% left_join(meters_class, by = "Meter")
  dataLog$Quant_Group <- quantity_class(dataLog$Quantity)
  
  glimpse(dataLog)
  rm(quantities, quantity, source_ids, quantity_ids, meters_class)
  ## write_feather(dataLog, "featherFiles/dataLog.feather")
}
