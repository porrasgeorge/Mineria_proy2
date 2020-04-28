library(shiny)
library(lubridate)
library(ggplot2)
library(dplyr) 
library(tidyr)
library(scales)
library(openxlsx)
source("DB.R")
source("methods.R")

end_dateCR <- floor_date(now(), "week")+ days(1)
start_dateCR <- end_dateCR - weeks(1)

dataLog  <- read_feather("featherFiles/dataLog.feather")
coop_mets <- dataLog %>%
    distinct(Cooperative, Meter, Quantity)%>%
    arrange(Cooperative, Meter, Quantity)

################################################################################################
ui <- fluidPage(
    titlePanel(title = h2("Conelectricas RL", align = "center")),
    h4("Reportes"),
    hr(),
    fluidRow(
        column(3,
            dateRangeInput("daterange", "Rango de Fecha",
                           start = start_dateCR,
                           end = end_dateCR,
                           ##min = min(Surveillance$Date),
                           ##max = max(Surveillance$Date),
                           separator = " - ", 
                           format = "dd/mm/yyyy",
                           startview = 'Week', 
                           language = 'es', 
                           weekstart = 1)
            ),
        column(3, 
               selectInput(inputId = 'coop_si',
                           label='Cooperativa',
                           choices=unique(coop_mets$Cooperative))
        ),
        column(3, 
               selectInput(inputId = 'source_si',
                           label='Medidor',
                           choices="",
                           selected = "")
        ),
        column(3, 
               selectInput(inputId = "quant_type", 
                           label="Variable", 
                           choices = "", 
                           selected = "")
        )
        ), ## fin de fluid row
    
    hr(),
        
    textOutput("message_text"),
    textOutput("message_text_V"),
    hr(),
    
    
    tabsetPanel(type = "tab", 
                tabPanel("Summary" ),
                tabPanel("Histogram2",
                         align="center",
                         br(),
                         sliderInput(inputId = "bins",
                                   label = "Numero de barras:",
                                   min = 10,
                                   max = 100,
                                   value = 50),
                         plotOutput("histo2")),
                tabPanel("Tabla", 
                         align="center",
                         ## h4("Tabla"),
                         tableOutput('table')),
                tabPanel("Histograma", 
                         plotOutput("histogram"))
                )

        
    )


################################################################################################

server <- function(input, output, session) {
    
###################################################################################### Reactivity
    
    
    observeEvent(input$coop_si,
                 updateSelectInput(session, "source_si", label='Medidor:', 
                                   choices = coop_mets$Meter[coop_mets$Cooperative == input$coop_si]))
    
    observeEvent(input$source_si,
                 updateSelectInput(session, "quant_type", label='Variable', 
                                   choices = group_VoltagesName(coop_mets$Quantity[coop_mets$Meter == input$source_si])))

    
    
    meter_data <- reactive({
        return(filter_DataDataSelection(dataLog, input$source_si, input$daterange, input$quant_type))
    })
    
    t_Nominal <- reactive({
        return(guess_Nominal(meter_data()$Value))
        })

    voltageTable <- reactive({
        return(voltage_Summary(meter_data(), t_Nominal()))
    })

    output$message_text <- renderText({
        if (is.null(voltageTable())){
            paste("No hay datos en el periodo seleccionado")
        }
        else {
            paste("Se tiene: ", sum(voltageTable()$Freq), " datos")
        }
    })

    output$message_text_V <- renderText({
        paste("Voltage nominal detectado: ", t_Nominal(), "V")
    })

###################################################################################### Table

        output$table <- renderTable(
            if (is.null(voltageTable())){
                return (NULL)
            }
            else {
                create_Percent_Table(voltageTable(), t_Nominal())
            },
            digits = 0,
            striped = TRUE,
            hover = TRUE,
            ##bordered = TRUE,
            width = '100%',
            align = 'c')

###################################################################################### Plot

        output$histogram <- renderPlot({
            if (is.null(voltageTable())){
                return (NULL)
            }
            else {
                return(create_Histo_Plot(voltageTable(),
                                         input$source_si))
            }
        })
        
        output$histo2 <- renderPlot({
            if (is.null(meter_data())){
                return (NULL)
            }
            else {
                x <- meter_data()$Value
                bins <- seq(min(x), max(x), length.out = input$bins + 1)
                
                return(hist(x, 
                            breaks = bins, 
                            col = "#75AADB", 
                            border = "blue",
                            xlab = "Voltaje",
                            ylab = "Frecuencia",
                            main = "Histograma de Voltaje"))
            }
        })
        
        

###################################################################################### downloads
#         output$downloadData <- downloadHandler(
#             filename = function() {
#                 paste(selected_sourceName_reactive(), ".xlsx", sep = "")
#             },
#             content = function(file) {
#                 saveWorkbook(create_Excel_file(create_Percent_Table(IonData_LineV(),
#                                                                     tensiones),
#                                                selected_sourceName_reactive()),
#                              file = file,
#                              overwrite = TRUE)
#             }
#             )

    ##output$message_text <- renderText(paste(input$selected_source, " -- "))
    ##output$table <- renderTable(IonData_LineV())
        }
shinyApp (ui = ui, server = server)