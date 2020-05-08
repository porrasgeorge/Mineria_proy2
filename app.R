library(shiny)
library(lubridate)
library(ggplot2)
library(dplyr) 
library(tidyr)
library(scales)
##library(openxlsx)
library(DT)
##library(shinythemes)
source("DB.R")
source("methods.R")

end_dateCR <- floor_date(now(), "week")+ days(1)
start_dateCR <- end_dateCR - weeks(1)

#dataLog  <- read_feather("featherFiles/dataLog.feather")
dataLog  <- read_feather("featherFiles/dataLog_big.feather")

coop_mets <- dataLog %>%
    distinct(Cooperative, Type, Meter, Quant_class, Quantity) %>%
    arrange(Cooperative, Type, Meter, Quant_class, Quantity)

glimpse(coop_mets)

################################################################################################
ui <- fluidPage( ##theme = shinytheme("united"),
    tags$head(HTML("<title>CDC Conelectricas</title> <link rel='icon' type='image/gif/png' href='ConeLogo.jpg'>")), 
    h4("Filtros:"),
    
    fluidRow(
        column(2,
               dateRangeInput("daterange", "Rango de Fecha",
                              start = start_dateCR,
                              end = end_dateCR,
                              separator = " - ", 
                              format = "dd/mm/yyyy",
                              startview = 'Week', 
                              language = 'es', 
                              weekstart = 1)
        ),
        column(2, 
               selectInput(inputId = 'coop_si',
                           label='Cooperativa',
                           choices=unique(coop_mets$Cooperative))
        ),
        column(2, 
               selectInput(inputId = 'type_si',
                           label='Tipo',
                           choices="",
                           selected = "")
        ),
        column(2, 
               selectInput(inputId = 'source_si',
                           label='Medidor',
                           choices="",
                           selected = "")
        ),
        column(2, 
               selectInput(inputId = "quant_type", 
                           label="Variable", 
                           choices = "", 
                           selected = "")
        ),
        column(1,offset = 1, 
               img(src='ConeLogo.jpg', style="display: block; margin-left: auto; margin-right: auto; width: 75px; height: 75px")
        )
    ), ## fin de fluid row

    ### Inicio de los tabs
    
    tabsetPanel(type = "tab",
                tabPanel("Voltaje",
                         br(),
                tabsetPanel(type = "tab",
                            tabPanel("Datos",
                                     br(),
                                     textOutput("message_text"),
                                     textOutput("message_text_V"),
                                     hr(),
                                     DTOutput('full_Table')),
                            tabPanel("Densidad",
                                     align="center",
                                     br(),
                                     column(3,
                                            sliderInput(inputId = "Hist1_minV",
                                                        label = "Voltaje Minimo",
                                                        min = 0,
                                                        max = 35000,
                                                        value = 0)
                                            ),
                                     plotOutput("volt_dens")
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
                            ) ## tabsetPanel_2
                ),  ## tabPanel Voltajes
                tabPanel("THDs", 
                         hr()),
                
                tabPanel("XYZ", 
                         hr()),
                
                tabPanel("ABCD", 
                         hr())
                
         ) ## tabsetPanel_1
    
) ## fluidpage


################################################################################################

server <- function(input, output, session) {
    
###################################################################################### Reactivity
    
    
    observeEvent(input$coop_si,
                 updateSelectInput(session, "type_si", label='Tipo', 
                                   choices = coop_mets$Type[coop_mets$Cooperative == input$coop_si]))
    
    observeEvent(input$type_si,
                 updateSelectInput(session, "source_si", label='Medidor', 
                                   choices = coop_mets$Meter[(coop_mets$Type == input$type_si) & 
                                                                 (coop_mets$Cooperative == input$coop_si)]))
    observeEvent(input$source_si,
                 updateSelectInput(session, "quant_type", label='Variable', 
                                   choices = coop_mets$Quant_class[(coop_mets$Type == input$type_si) & 
                                                                 (coop_mets$Cooperative == input$coop_si) & 
                                                                 (coop_mets$Meter == input$source_si)]))
    
                                   ##choices = group_VoltagesName(coop_mets$Quantity[coop_mets$Meter == input$source_si])))
    

    observeEvent( meter_data(), {
##        print("meter_data va a ser modificada")
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
##        print("meter_data ha sido modificada")
    })
    
    meter_data <- reactive({
        quantities_filtered <- coop_mets$Quantity[(coop_mets$Type == input$type_si) & 
                                                 (coop_mets$Cooperative == input$coop_si) & 
                                                     (coop_mets$Meter == input$source_si)& 
                                                     (coop_mets$Quant_class == input$quant_type)]
        return(filter_DataDataSelection(dataLog, input$source_si, input$daterange, quantities_filtered))
        
##        return(filter_DataDataSelection(dataLog, input$source_si, input$daterange, input$quant_type))
    })
    
    t_Nominal <- reactive({
        return(guess_Nominal(meter_data()$Value))
        })

    voltageTable <- reactive({
        return(voltage_Summary(meter_data(), t_Nominal()))
    })
    
    unbalance_table<- reactive({
        return()
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
                in_table$Type <- NULL
                in_table$Meter <- NULL
                in_table$
                col_numb <- ncol(in_table)
                return(in_table)
            },
            filter = "none",
            selection = "none", 
            options = list(pageLength = 15, columnDefs = list(list(className = 'dt-center', targets = "_all")))
            )

        ##########################################################################################
        ## Voltage Predefined Histogram
        output$histogram <- renderPlot({
            if (is.null(voltageTable())){
                return (NULL)
            }
            else {
                return(create_Histo_Plot(voltageTable(),
                                         input$source_si))
            }
        })

        ##########################################################################################
        ## Voltage Density Plot
        output$volt_dens <- renderPlot({
            if (is.null(meter_data())){
                return (NULL)
            }
            else {
                in_data <- meter_data()
                plot_data <- in_data[in_data$Value > input$Hist1_minV,]
                lineas <- plot_data %>% group_by(Quantity) %>% summarise(v = mean(Value))
                if (nrow(plot_data) <10){
                    return(NULL)
                }
                else{
                    hplot <- ggplot(plot_data, aes(x=Value, fill = Quantity)) + 
                           geom_density(alpha = 0.6) +
                           geom_vline(data=lineas, aes(xintercept=v, color=Quantity), size = 2)
                    
                    return(hplot)
                }
            }
        })
        
        ##########################################################################################
        ## Voltage Box Plot
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