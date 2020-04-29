library(shiny)
library(lubridate)
library(ggplot2)
library(dplyr) 
library(tidyr)
library(scales)
library(openxlsx)
library(DT)
library(shinythemes)
source("DB.R")
source("methods.R")

end_dateCR <- floor_date(now(), "week")+ days(1)
start_dateCR <- end_dateCR - weeks(1)

#dataLog  <- read_feather("featherFiles/dataLog.feather")
dataLog  <- read_feather("featherFiles/dataLog_v2.feather")
coop_mets <- dataLog %>%
    distinct(Cooperative, Meter, Quantity)%>%
    arrange(Cooperative, Meter, Quantity)

################################################################################################
ui <- fluidPage(theme = shinytheme("united"),
    titlePanel(title = img(src='logo.png', style="display: block; margin-left: auto; margin-right: auto;"), 
               windowTitle = "CDC Conelectricas"),
    tags$head(HTML("<link rel='icon' type='image/gif/png' href='ConeLogo.jpg'>")), 
    
    hr(),
    h4("Filtros:"),
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
                tabPanel("Tabla Original" ,
                         DTOutput('full_Table')),
                tabPanel("Histograma",
                         align="center",
                         br(),
                         
                         column(3,
                                sliderInput(inputId = "Hist1_bins",
                                            label = "Numero de barras:",
                                            min = 10,
                                            max = 100,
                                            value = 50)
                                ),
                         
                         column(3,
                                
                                sliderInput(inputId = "Hist1_minV",
                                            label = "Voltaje Minimo",
                                            min = 0,
                                            max = 35000,
                                            value = 0)
                                ),
                         
                         
                         plotOutput("histo2")
                         ),
                tabPanel("Boxplot",
                         align="center",
                         br(),
                         
                         column(3,
                                
                                sliderInput(inputId = "Box_minV",
                                            label = "Eliminar Valores menores a:",
                                            min = 0,
                                            max = 35000,
                                            value = 0)
                         ),
                         plotOutput("box_volt")
                ),
                
                
                
                
                
                tabPanel("Clasificación", 
                         align="center",
                         ## h4("Tabla"),
                         tableOutput('table')),
                tabPanel("Histograma de Clasificación", 
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

    observeEvent( meter_data(), {
        print("meter_data va a ser modificada")
        in_data <- meter_data()$Value
        if (length(in_data) >10){
            min_val = round(min(in_data), -2)
            max_val = round((min_val +(max(in_data) - min_val)*0.95), -2)
            updateSliderInput(session, inputId = "Hist1_minV",
                              label = "Voltaje Minimo",
                              min = min_val,
                              max = max_val,
                              value = min_val)
            
            updateSliderInput(session, inputId = "Box_minV",
                              label = "Eliminar Valores menores a:",
                              min = min_val,
                              max = max_val,
                              value = min_val)
        }
        
        
        print("meter_data ha sido modificada")
        
    })
    
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
## Tablas
    
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
            align = 'c'
            )

                
        output$full_Table <- renderDT(
            if (is.null(meter_data())){
                NULL
            }
            else {
                in_table <- meter_data()
                in_table <- in_table %>% spread(Quantity, value = Value, fill = 0)
                in_table$Cooperative <- NULL
                in_table$Meter <- NULL
                glimpse(in_table)
                return(in_table)
            },
            filter = "top",
            options = list(pageLength = 10, columnDefs = list(list(className = 'dt-center', targets = 1:4)))
            )
        
        

###################################################################################### Plot
## Histogramas
        
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
                in_data <- meter_data()$Value
                plot_data <- in_data[in_data > input$Hist1_minV]
                if (length(plot_data) <10){
                    return(NULL)
                }
                else{
                bins <- seq(min(plot_data), max(plot_data), length.out = input$Hist1_bins + 1)
                return(hist(plot_data,
                            breaks = bins,
                            col = "#75AADB",
                            border = "blue",
                            xlab = "Voltaje",
                            ylab = "Frecuencia",
                            main = "Histograma de Voltaje"))
                }
            }
        })

        output$box_volt <- renderPlot({
            if (is.null(meter_data())){
                return (NULL)
            }
            else {
                in_data <- meter_data()
                plot_data <- in_data %>% filter (Value > as.numeric(input$Box_minV))
                if (nrow(plot_data) <10){
                    return(NULL)
                }
                else{
                    print(paste("rows ", nrow(plot_data))) 
                    bp <- ggplot(plot_data, aes(Quantity, Value)) + 
                        geom_boxplot(aes(colour = Quantity))
                    return(bp)
                }
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