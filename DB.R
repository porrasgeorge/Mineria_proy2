library(RODBC)

DBget_Sources <- function(){
  channel <- odbcConnect("SQL_ION", uid="sa", pwd="Con3$adm.")
  sources <- sqlQuery(channel , "select top 100 ID, Name, DisplayName from Source where Name like 'Coopeguanacaste.%'")
  sources$Name <- gsub("Coopeguanacaste.", '', sources$Name)
  sources <- sources %>% filter(ID > 47)
  sources$Name <- gsub("_"," ", sources$Name)
  odbcClose(channel)
  
  return(sources)
}

DBget_DataLineVoltage <- function(sources, selected_source, daterange, tensiones){
  print("i have been called ")

  start_date_UTC <- with_tz(daterange[1], tzone = "UTC")
  end_date_UTC <- with_tz(daterange[2], tzone = "UTC")
  
  
  channel <- odbcConnect("SQL_ION", uid="sa", pwd="Con3$adm.")
  quantity <- sqlQuery(channel , "select top 1500000 ID, Name from Quantity where Name like 'Voltage%'")
  dataLog <- sqlQuery(channel , paste0("select top 500000 * from DataLog2 where ",
                            "SourceID in (", selected_source, ")",
                            " and QuantityID in (167, 173, 176)",
                            " and TimestampUTC >= '", start_date_UTC, "'",
                            " and TimestampUTC < '", end_date_UTC, "'"))
  odbcClose(channel)
  
  if (nrow(dataLog) == 0) {
    return (NULL)
  }
  
  quantity <- quantity %>% filter(grepl("^Voltage Phases [ABC][ABC] Mean$", Name))
  quantity$Name <- c('Vab', 'Vbc', 'Vca')
  
  dataLog$TimestampUTC <- as_datetime(dataLog$TimestampUTC)
  dataLog$TimestampCR <- with_tz(dataLog$TimestampUTC, tzone = "America/Costa_Rica") 
  dataLog$TimestampUTC <- NULL
  dataLog$ID <- NULL
  
  dataLog <- dataLog %>% left_join(quantity, by = c('QuantityID' = "ID")) %>%
    left_join(sources, by = c('SourceID' = "ID"))
  
  names(dataLog)[names(dataLog) == "Name.x"] <- "Quantity"
  names(dataLog)[names(dataLog) == "Name.y"] <- "Meter"
  dataLog$SourceID <- NULL
  dataLog$QuantityID <- NULL
  dataLog$DisplayName <- NULL
  
  
  dataLog <- dataLog %>% mutate(classif = case_when(Value < tensiones["limit087"] ~ "TN087",
                                                    Value < tensiones["limit091"] ~ "TN087_091",
                                                    Value < tensiones["limit093"] ~ "TN091_093",
                                                    Value < tensiones["limit095"] ~ "TN093_095",
                                                    Value < tensiones["limit105"] ~ "TN095_105",
                                                    Value < tensiones["limit107"] ~ "TN105_107",
                                                    Value < tensiones["limit109"] ~ "TN107_109",
                                                    Value < tensiones["limit113"] ~ "TN109_113",
                                                    TRUE ~ "TN113"
  ))
  
  dataLog$classif <- factor(dataLog$classif, levels = list("TN087", "TN087_091", "TN091_093", "TN093_095", "TN095_105", "TN105_107", "TN107_109", "TN109_113", "TN113"))
  dataLog_table <- as.data.frame(table(dataLog$classif, dataLog$Quantity, dnn = c("Classif", "Quantity")))
  sum_total <- sum(dataLog_table$Freq)/3
  dataLog_table$Perc <- dataLog_table$Freq/sum_total
  
  return(dataLog_table)
}
