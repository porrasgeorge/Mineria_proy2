library(RODBC)
library(feather)
library(tidyverse)
library(shiny)

DB_getDataSaveFile <- function(){

  final_dateCR <- floor_date(now(), "day") ## corte hasta hoy
  initial_dateCR <- final_dateCR - months(2) ## 3 meses hacia atras 
  initial_date <- with_tz(initial_dateCR, tzone = "UTC") 
  final_date <- with_tz(final_dateCR, tzone = "UTC")
  
  channel <- odbcConnect("SQL_ION", uid="sa", pwd="Con3$adm.")
  sources <- sqlQuery(channel , "select top 1000 ID, Name from Source")
  sources <- sources %>% 
    separate(Name, c("Cooperative", "Meter"), "\\.") %>%
    filter(!Cooperative %in% c("LOGINSERTER", "QUERYSERVER", "VIP"))
  
  quantity <- sqlQuery(channel , "select top 1500 ID, Name from Quantity where Name like 'Voltage%'")
  quantities <- quantity %>% 
    filter(grepl("^Voltage Phases [ABC][ABC] Mean$", Name) |
             grepl("^Voltage on Input V[123] Mean - Power Quality Monitoring$", Name)) %>%
    arrange(ID)
  quantities$Name <- c('Vab', 'Vbc', 'Vca', 'Van', 'Vbn', 'Vcn')

  source_ids <- paste0(sources$ID, collapse = ",")
  quantity_ids <- paste0(quantities$ID, collapse = ",")
  
  dataLog <- sqlQuery(channel , paste0("select top 5000000 * from dataLog2 where ",
                                       "SourceID in (", source_ids, ")",
                                       " and QuantityID in (", quantity_ids, ")",
                                       " and TimestampUTC >= '", initial_date, "'",
                                       " and TimestampUTC < '", final_date, "'"))
  
  odbcCloseAll()

  dataLog$TimestampUTC <- as_datetime(dataLog$TimestampUTC)
  dataLog$TimestampCR <- with_tz(dataLog$TimestampUTC, tzone = "America/Costa_Rica") 
  dataLog$TimestampUTC <- NULL
  dataLog$ID <- NULL
  
  dataLog <- dataLog %>% left_join(quantities, by = c('QuantityID' = "ID")) %>%
    left_join(sources, by = c('SourceID' = "ID"))
  
  dataLog$Name <- as.factor(dataLog$Name)
  dataLog$Cooperative <- as.factor(dataLog$Cooperative)
  dataLog$Meter <- gsub(pattern = "_", replacement = " ", dataLog$Meter)
  dataLog$Meter <- as.factor(dataLog$Meter)
  names(dataLog)[names(dataLog) == "Name"] <- "Quantity"
  dataLog$SourceID <- NULL
  dataLog$QuantityID <- NULL
  
  rm(quantities, quantity, source_ids, quantity_ids)
  #write_feather(dataLog, "featherFiles/dataLog.feather")
}

