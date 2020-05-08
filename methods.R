filter_DataDataSelection <- function(dataToFilter, Source, dateRange, quantities){
  print("Filter_DataDataSelection called ...")

  # quantities <- vector()
  # if (quantType == "Vline"){
  #   quantities <- c('Vab', 'Vbc', 'Vca')
  # }
  # else if (quantType == "Vphase"){
  #   quantities <- c('Van', 'Vbn', 'Vcn')
  # }
  
  lineV <- dataToFilter %>% 
    filter(Meter == Source,
           Quantity %in% quantities, 
           TimestampCR >= dateRange[1], 
           TimestampCR < dateRange[2])
  
  lineV$Quantity <- factor(lineV$Quantity, levels = quantities)
  lineV$TimestampCR <- format(lineV$TimestampCR,'%d-%m-%Y %H:%M')
  
  lineV <- lineV %>% arrange(TimestampCR, Quantity)
  lineV$Value <- round(lineV$Value, 1)

  print("Filter_DataDataSelection OK ...")
  return(lineV)
}


####################################################################################################

voltage_Summary <- function(data, t_Nom){
  print("voltage_Summary called ...")

  if (nrow(data) == 0) {
    return (NULL)
  }

  tensiones <- c(t_Nom, 0.87*t_Nom, 0.91*t_Nom,0.93*t_Nom,0.95*t_Nom,1.05*t_Nom,1.07*t_Nom,1.09*t_Nom,1.13*t_Nom)
  names(tensiones) <- c("Nom", "limit087" ,"limit091","limit093","limit095" ,"limit105" ,"limit107" ,"limit109" ,"limit113")
  
  voltage_DF <- data %>% mutate(Classif = case_when(Value < tensiones["limit087"] ~ "TN087",
                                                Value < tensiones["limit091"] ~ "TN087_091",
                                                Value < tensiones["limit093"] ~ "TN091_093",
                                                Value < tensiones["limit095"] ~ "TN093_095",
                                                Value < tensiones["limit105"] ~ "TN095_105",
                                                Value < tensiones["limit107"] ~ "TN105_107",
                                                Value < tensiones["limit109"] ~ "TN107_109",
                                                Value < tensiones["limit113"] ~ "TN109_113",
                                                TRUE ~ "TN113"
  ))
  
  voltage_DF$Classif <- factor(voltage_DF$Classif, levels = list("TN087", "TN087_091", "TN091_093", "TN093_095", "TN095_105", "TN105_107", "TN107_109", "TN109_113", "TN113"))
  voltage_table <- as.data.frame(table(voltage_DF$Classif, voltage_DF$Quantity, dnn = c("Classif", "Quantity")))
  
  countsV <- voltage_table %>% group_by(Quantity) %>%
    summarise(CountSum = sum(Freq))
  
  voltage_table <- voltage_table %>% left_join(countsV, by = "Quantity")
  voltage_table$Perc <- if_else(voltage_table$CountSum == 0, 0, voltage_table$Freq/voltage_table$CountSum)
  
  print("voltage_Summary OK ...")
  return(voltage_table)
}
# 
# group_VoltagesName <- function(volt_list){
#   print("group_VoltagesName called ...")
#   volt_group <- vector()
#   if ("Vab" %in% volt_list){
#     volt_group <- c(volt_group, "Vline")
#   }
#   if ("Van" %in% volt_list){
#     volt_group <- c(volt_group, "Vphase")
#   }
#   return(volt_group)
# }


guess_Nominal <- function(var_values){
  print("guess_Nominal called ...")
  var_values <- var_values[var_values >100]
  avg <- mean(var_values)
  Nominal = case_when(avg < 16000 ~ 14376,
                      avg < 23000 ~ 20207,
                      avg < 29000 ~ 24900,
                      TRUE ~ 35000
  )
  return(Nominal)
}



create_Percent_Table <- function(dataLog_table, t_Nom){
  print("create_Percent_Table called ...")
  dataLog_table$Perc <- label_percent(accuracy = 0.01)(dataLog_table$Perc)
  dataLog_table$Freq <- as.integer(dataLog_table$Freq)
  
  tensiones <- c(t_Nom, 0.87*t_Nom, 0.91*t_Nom,0.93*t_Nom,0.95*t_Nom,1.05*t_Nom,1.07*t_Nom,1.09*t_Nom,1.13*t_Nom)
  names(tensiones) <- c("Nom", "limit087" ,"limit091","limit093","limit095" ,"limit105" ,"limit107" ,"limit109" ,"limit113")
  
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

create_Histo_Plot <- function(IonData_V, title){
  p <- ggplot(IonData_V, aes(x = Classif, y = Perc, label = Perc, fill = Quantity )) +
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
  print("create_Excel_file called ...")
  wb <- createWorkbook()
  addWorksheet(wb, ws_name)
  setColWidths(wb, ws_name, cols = c(1:10), widths = c(20, rep(15, 9) ))
  writeDataTable(wb, ws_name, x = table, startRow = 1, rowNames = F, tableStyle = "TableStyleMedium1")
  return(wb)
  
  
}

meter_classes <- function(meters){
  meters_class <- as.data.frame(unique(meters), nm = ("Meter"))
  meters_class$Type <- "Distribucion"
  meters_class$Type <- factor(meters_class$Type, levels = c("Generacion", "Distribucion"))
  
  meter_Gen <- c("BIJAGUA CANALETE L2",
                 "BIJAGUA MIRAVALLES L1",
                 "BIJAGUA U1",
                 "BIJAGUA U2",
                 "Cacao",
                 "Cacao Nicoya",
                 "Cacao StaCruz",
                 "Canalete Unidad1",
                 "Canalete Unidad2",
                 "JUANILAMA PRINCIPAL",
                 "MIRAVALLES BIJAGUA",
                 "MIRAVALLES CANALETE",
                 "PERN",
                 "ICE PELS SR1",
                 "PELS bck")
  
  meters_class[meters_class$Meter %in% meter_Gen,]$Type <- "Generacion"
  return(meters_class)
}

### constant table for fast clasification of variables

quant_classes <- data.frame(Quantity = c("Vab", 
                                         "Vbc", 
                                         "Vca", 
                                         "Van", 
                                         "Vbn", 
                                         "Vcn",
                                         "Reactive Power", 
                                         "Active Power",
                                         "Power Factor Lagging AVG",
                                         "Power Factor Lagging MAX",
                                         "Power Factor Leading AVG",
                                         "Power Factor Leading MAX",
                                         "Voltage Unbalance AVG", 
                                         "Voltage Unbalance MAX",
                                         "Current IA THD AVG",
                                         "Current IB THD AVG",
                                         "Current IC THD AVG",
                                         "Current IA THD MAX",
                                         "Current IB THD MAX",
                                         "Current IC THD MAX",
                                         "Voltage V1 THD AVG",
                                         "Voltage V2 THD AVG",
                                         "Voltage V3 THD AVG",
                                         "Voltage V1 THD MAX",
                                         "Voltage V2 THD MAX",
                                         "Voltage V3 THD MAX"))


quant_classes$Quant_class <-  c("Vline", 
                                "Vline", 
                                "Vline", 
                                "Vphase", 
                                "Vphase", 
                                "Vphase",
                                "Power",
                                "Power",
                                "Power Factor",
                                "Power Factor",
                                "Power Factor",
                                "Power Factor",
                                "Vunbalance",
                                "Vunbalance",
                                "I THD",
                                "I THD",
                                "I THD",
                                "I THD",
                                "I THD",
                                "I THD",
                                "V THD",
                                "V THD",
                                "V THD",
                                "V THD",
                                "V THD",
                                "V THD")
  
quant_classes$Quant_class <- factor(quant_classes$Quant_class)
quant_classes$Quantity <- factor(quant_classes$Quantity)
