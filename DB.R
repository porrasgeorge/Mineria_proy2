library(RODBC)
library(feather)
library(tidyverse)
library(shiny)
library(lubridate)
source("methods.R")

DB_getDataSaveFile <- function() {
  final_dateCR <- floor_date(now(), "day") ## corte hasta hoy
  #initial_dateCR <- final_dateCR - months(6) ## 3 meses hacia atras
  initial_dateCR <- final_dateCR - days(18) ## 3 meses hacia atras
  
  initial_date <- with_tz(initial_dateCR, tzone = "UTC")
  final_date <- with_tz(final_dateCR, tzone = "UTC")
  
  channel <- odbcConnect("SQL_ION", uid = "R", pwd = "Con3$adm.")
  sources <-
    sqlQuery(channel , "select top 1000 ID, Name from Source")
  sources <- sources %>%
    separate(Name, c("Cooperative", "Meter"), "\\.") %>%
    filter(!Cooperative %in% c("LOGINSERTER", "QUERYSERVER", "VIP"))
  
  quantity <-
    sqlQuery(channel , "select top 10000 ID, Name from Quantity")
  quantities <- quantity %>%
    filter(
      grepl("^Voltage Phases [ABC][ABC] Mean$", Name) |
        grepl("^Voltage on Input V[123] Mean - Power Quality Monitoring$", Name) |
        grepl("^(Rea|A)ctive Power Mean$", Name) |
        grepl("^Power Factor (Lagging|Leading) Mean$", Name) |
        grepl("^Voltage Unbalance Mean$", Name) |
        grepl("^Voltage Total Harmonic Distortion 10-minute Mean on Input V[123]$",Name) |
        grepl("^Voltage Total Harmonic Distortion Mean on Input V[123]$",Name) |
        grepl("^Current Phase [ABC] Mean$",Name)
      
    ) %>%
    arrange(ID)
  quantities$Name <- c(
    'Ia',
    'Ib',
    'Ic',
    'Reactive Power',
    'Active Power',
    'Power Factor Lagging',
    'Power Factor Leading',
    'Voltage Unbalance',
    'Va THD 1hr',
    'Vb THD 1hr',
    'Vc THD 1hr',
    'Vab',
    'Vbc',
    'Vca',
    'Van',
    'Vbn',
    'Vcn',
    'Va THD',
    'Vb THD',
    'Vc THD'
  )
  
  source_ids <- paste0(sources$ID, collapse = ",")
  quantity_ids <- paste0(quantities$ID, collapse = ",")
  
  dataLog <-
    sqlQuery(
      channel ,
      paste0(
        "select top 0 * from dataLog2 where ",
        "SourceID in (",
        source_ids,
        ")",
        " and QuantityID in (",
        quantity_ids,
        ")",
        " and TimestampUTC >= '",
        initial_date,
        "'",
        " and TimestampUTC < '",
        initial_date,
        "'"
      )
    )
  
  while (initial_date < final_date) {
    start_time <- Sys.time()
    
    end_day_date <- initial_date + days(1)
    data_read <-
      sqlQuery(
        channel ,
        paste0(
          "select top 500000 * from dataLog2 where ",
          "SourceID in (",
          source_ids,
          ")",
          " and QuantityID in (",
          quantity_ids,
          ")",
          " and TimestampUTC >= '",
          initial_date,
          "'",
          " and TimestampUTC < '",
          end_day_date,
          "'"
        )
      )
    
    dataLog <- rbind(dataLog, data_read)
    
    end_time <- Sys.time()
    print(paste(
      "Done date : ",
      initial_date,
      "time needed: ",
      end_time - start_time
    ))
    print(end_time - start_time)
    
    initial_date <- end_day_date
  }
  
  odbcCloseAll()
  
  dataLog$TimestampUTC <- as_datetime(dataLog$TimestampUTC, tz = "UTC")
  dataLog$TimestampCR <-
    with_tz(dataLog$TimestampUTC, tzone = "America/Costa_Rica")
  dataLog$TimestampUTC <- NULL
  dataLog$ID <- NULL
  
  dataLog <-
    dataLog %>% left_join(quantities, by = c('QuantityID' = "ID")) %>%
    left_join(sources, by = c('SourceID' = "ID"))
  
  dataLog$Name <- as.factor(dataLog$Name)
  dataLog$Cooperative <- as.factor(dataLog$Cooperative)
  dataLog$Meter <-
    gsub(pattern = "_", replacement = " ", dataLog$Meter)
  dataLog$Meter <- as.factor(dataLog$Meter)
  names(dataLog)[names(dataLog) == "Name"] <- "Quantity"
  dataLog$SourceID <- NULL
  dataLog$QuantityID <- NULL
  
  meters_class  <- meter_classes(dataLog$Meter)
  dataLog <- dataLog %>% left_join(meters_class, by = "Meter")
  dataLog <- dataLog %>% left_join(quant_classes, by = "Quantity")
  
  
  rm(quantities, quantity, source_ids, quantity_ids, meters_class)
  write_feather(dataLog, "featherFiles/dataLog_big3.feather")
}

