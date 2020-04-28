library(RODBC)
library(feather)
library(tidyverse)

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




####################################################################################################

DBget_Voltage <- function(selected_source, daterange, quant_type){
  
  print("get voltages called ")
  
  quantities <- vector()
  if (quant_type == "Vline"){
    quantities <- c('Vab', 'Vbc', 'Vca')
  }
  else if (quant_type == "Vphase"){
    quantities <- c('Van', 'Vbn', 'Vcn')
  }
  
  lineV <- dataLog %>% 
    filter(Meter == selected_source,
           Quantity %in% quantities, 
           TimestampCR >= daterange[1], 
           TimestampCR < daterange[2])

  lineV$Quantity <- factor(lineV$Quantity, levels = quantities)
  t_Nom = guess_Nominal(lineV$Value)
  
  T_Nominal <<- t_Nom
  print(paste("V= ", t_Nom))
  ##T_Nominal(t_Nom)
  
  tensiones <- c(t_Nom, 0.87*t_Nom, 0.91*t_Nom,0.93*t_Nom,0.95*t_Nom,1.05*t_Nom,1.07*t_Nom,1.09*t_Nom,1.13*t_Nom)
  names(tensiones) <- c("Nom", "limit087" ,"limit091","limit093","limit095" ,"limit105" ,"limit107" ,"limit109" ,"limit113")

  if (nrow(lineV) == 0) {
    return (NULL)
  }
  

  lineV <- lineV %>% mutate(classif = case_when(Value < tensiones["limit087"] ~ "TN087",
                                                Value < tensiones["limit091"] ~ "TN087_091",
                                                Value < tensiones["limit093"] ~ "TN091_093",
                                                Value < tensiones["limit095"] ~ "TN093_095",
                                                Value < tensiones["limit105"] ~ "TN095_105",
                                                Value < tensiones["limit107"] ~ "TN105_107",
                                                Value < tensiones["limit109"] ~ "TN107_109",
                                                Value < tensiones["limit113"] ~ "TN109_113",
                                                TRUE ~ "TN113"
  ))
  
  lineV$classif <- factor(lineV$classif, levels = list("TN087", "TN087_091", "TN091_093", "TN093_095", "TN095_105", "TN105_107", "TN107_109", "TN109_113", "TN113"))
  lineV_table <- as.data.frame(table(lineV$classif, lineV$Quantity, dnn = c("Classif", "Quantity")))
  
  countsV <- lineV_table %>% group_by(Quantity) %>%
    summarise(CountSum = sum(Freq))
  
  lineV_table <- lineV_table %>% left_join(countsV, by = "Quantity")
  lineV_table$Perc <- if_else(lineV_table$CountSum == 0, 0, lineV_table$Freq/lineV_table$CountSum)
  
  ##browser()
  
  ##sum_total <- floor(sum(lineV_table$Freq)/3)
  #lineV_table$Perc <- lineV_table$Freq/sum_total
  
  return(lineV_table)
}
