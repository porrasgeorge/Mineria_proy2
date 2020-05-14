library(RODBC)
library(lubridate)
library(dplyr) 
library(tidyr)


final_dateCR <- floor_date(now(), "day") ## corte hasta hoy
initial_dateCR <- final_dateCR - days(1) ## 3 meses hacia atras 
initial_date <- with_tz(initial_dateCR, tzone = "UTC") 
final_date <- with_tz(final_dateCR, tzone = "UTC")

channel <- odbcConnect("SQL_ION", uid="R", pwd="Con3$adm.")
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


d3 <- d2 %>% 
  filter(Meter == "Coopealfaroruiz.Sub_Laguna") %>%
  group_by(Quantity) %>%
  summarise(avg = mean(Cantidad))


d4 <- dataLog %>% filter(Meter == "Cacao", Quant_class == "V THD")
d4$TimestampCR <- floor_date(d4$TimestampCR, unit = "minute")
d5 <- d4 %>% distinct(TimestampCR, Quantity, .keep_all = TRUE)


glimpse(d4)


d5 <- dataLog %>% filter(Meter == "Tribunales Jicaral", Quant_class == "V THD 1hr")
d6 <- dataLog %>% filter(Meter == "Tribunales Jicaral", Quant_class == "Vline")





x <- datatable(iris) %>% 
  formatStyle('Sepal.Length', fontWeight = styleInterval(5, c('normal', 'bold'))) %>%
  formatStyle(
    'Sepal.Width',
    color = styleInterval(c(3.4, 3.8), c('white', 'blue', 'red')),
    backgroundColor = styleInterval(3.4, c('gray', 'yellow'))
  ) %>%
  formatStyle(
    'Petal.Length',
    background = styleColorBar(iris$Petal.Length, 'steelblue'),
    backgroundSize = '100% 90%',
    backgroundRepeat = 'no-repeat',
    backgroundPosition = 'center'
  ) %>%
  formatStyle(
    'Species',
    transform = 'rotateX(45deg) rotateY(20deg) rotateZ(30deg)',
    backgroundColor = styleEqual(
      unique(iris$Species), c('lightblue', 'lightgreen', 'lightpink')
    )
  )

renderDT(x)


