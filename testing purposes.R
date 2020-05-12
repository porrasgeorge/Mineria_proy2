library(shiny)
library(lubridate)
library(ggplot2)
library(dplyr) 
library(tidyr)
##library(openxlsx)
library(DT)
source("DB.R")
source("methods.R")

end_dateCR <- floor_date(now(), "week", week_start = 1)
start_dateCR <- end_dateCR - weeks(1)

#dataLog  <- read_feather("featherFiles/dataLog.feather")
dataLog  <- read_feather("featherFiles/dataLog_big2.feather")

coop_mets <- dataLog %>%
    distinct(Cooperative, Type, Meter, Quant_class, Quantity) %>%
    arrange(Cooperative, Type, Meter, Quant_class, Quantity)

################################################################################################
ui <- fluidPage( ##theme = shinytheme("united"),
    tags$head(HTML("<title>CDC Conelectricas</title> <link rel='icon' type='image/gif/png' href='ConeLogo.jpg'>")), 
    h4("Filtros:"),
    
    fluidRow(
        column(3,
               dateRangeInput("daterange", "Rango de Fecha",
                              start = start_dateCR,
                              end = end_dateCR,
                              separator = " - ", 
                              format = "dd/mm/yyyy",
                              startview = 'Week', 
                              language = 'es', 
                              weekstart = 1)
        ),
        column(3, 
               selectInput(inputId = 'coop_si',
                           label='Cooperativa',
                           choices= unique(coop_mets$Cooperative))
        ),
        column(3, 
               selectInput(inputId = 'type_si',
                           label='Tipo',
                           choices="",
                           selected = "")
        ),
        column(3, 
               selectInput(inputId = 'source_si',
                           label='Medidor',
                           choices="",
                           selected = "")
        ),
        # column(3, 
        #        selectInput(inputId = "quant_type", 
        #                    label="Variable", 
        #                    choices = "", 
        #                    selected = "")
        # ),
        column(3,
               br(),
               actionButton("go", "Go!")
        ),
        column(1,
               img(src='ConeLogo.jpg', style="display: block; margin-left: auto; margin-right: auto; width: 75px; height: 75px")
        )
    ), ## fin de fluid row
 
    ### Inicio de los tabs
    
    tabsetPanel(type = "tab",
                tabPanel("Vphase",
                         # br(),
                         textOutput("Vphase_NODATA"),
                         # textOutput("message_text_V"),
                         #hr(),
                         DTOutput('Vphase_DataTable'),
                         hr(),
                         column(3,
                                
                                sliderInput(inputId = "Box_minVphase",
                                            label = "Eliminar Valores menores a:",
                                            min = 0,
                                            max = 35000,
                                            value = 0)
                                ),
                         plotOutput("box_Vphase"),
                         hr(),
                         plotOutput("dens_Vphase")
                         ),
                
                tabPanel("Vline",
                         DTOutput('Vline_DataTable')),
                
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
    
) ## fluidpage


################################################################################################

server <- function(input, output, session) {
    
###################################################################################### Reactivity
    
    
    observeEvent(input$coop_si, {
        updateSelectInput(session, "type_si", 
                          label='Tipo',
                          choices = coop_mets$Type[coop_mets$Cooperative == input$coop_si])
        updateSelectInput(session, "source_si", label='Medidor', 
                          choices = coop_mets$Meter[(coop_mets$Type == input$type_si) & 
                                                        (coop_mets$Cooperative == input$coop_si)])
    })
    
    observeEvent(input$type_si,{
                 updateSelectInput(session, "source_si", label='Medidor', 
                                   choices = coop_mets$Meter[(coop_mets$Type == input$type_si) & 
                                                                 (coop_mets$Cooperative == input$coop_si)])
                 })
    # observeEvent(input$source_si,
    #              updateSelectInput(session, "quant_type", label='Variable', 
    #                                choices = coop_mets$Quant_class[(coop_mets$Type == input$type_si) & 
    #                                                              (coop_mets$Cooperative == input$coop_si) & 
    #                                                              (coop_mets$Meter == input$source_si)]))
    
                                   ##choices = group_VoltagesName(coop_mets$Quantity[coop_mets$Meter == input$source_si])))

    meter_data <- eventReactive(input$go, {
        return(filter_DataDataSelection(dataLog, input$source_si, input$daterange))
    })
        
    Vphase_Data <- reactive({
        data <- meter_data() %>% filter(Quant_class == "Vphase")
        return(data)
    })  
    
    Vline_Data <- reactive({
        data <- meter_data() %>% filter(Quant_class == "Vline")
        return(data)
    })
 
    
    
 ## Actualizar los Input Sliders      
    observeEvent( Vphase_Data(), {
        print("meter_data va a ser modificada")
        in_data <- Vphase_Data()$Value
        if (length(in_data) >10){
            min_val = plyr::round_any(min(in_data), 100, f = floor)
            max_val = plyr::round_any((min_val +(max(in_data) - min_val)*0.95), 100,  f = floor)
            updateSliderInput(session, inputId = "Box_minVphase",
                              label = "Voltaje Minimo",
                              min = min_val,
                              max = max_val,
                              value = min_val)
        }



       print("meter_data ha sido modificada")
 })


    Vphase_Nominal <- reactive({
        return(guess_Nominal(Vphase_Data()$Value))
    })
    
    Vline_Nominal <- reactive({
        return(guess_Nominal(Vline_Data()$Value))
    })

    
    
    
    
        
    voltageTable <- reactive({
        return(voltage_Summary(meter_data(), t_Nominal()))
    })
    
    unbalance_table<- reactive({
        return()
    })
    
    output$Vphase_NODATA <- renderText({
        if (nrow(Vphase_Data()) < 2){
            paste("No hay datos en el periodo seleccionado")
        }
        else {
            paste("Se tiene: ", nrow(Vphase_Data()), " datos")
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

    ## Phase Voltage Table
        output$Vphase_DataTable <- renderDT({
            if (nrow(Vphase_Data()) < 2){
                shiny::showNotification("No data", type = "error")
                NULL
            }
            else {
                in_table <- Vphase_Data() %>% spread(Quantity, value = Value, fill = 0)
                in_table$Vavg <- round(rowMeans(in_table[, c("Van", "Vbn","Vcn")]), 2)
                in_table$Cooperative <- NULL
                in_table$Type <- NULL
                in_table$Meter <- NULL
                in_table$Quant_class <- NULL
                return(in_table)
            }
            },
            filter = "bottom",
            selection = "none",
            options = list(pageLength = 10, columnDefs = list(list(className = 'dt-center', targets = "_all")))
            )

        
        
        ## Line Voltage Table
        output$Vline_DataTable <- renderDT({
            if (nrow(Vline_Data()) < 2){
                shiny::showNotification("No data", type = "error")
                NULL
            }
            else {
                
                in_table <- Vline_Data() %>% spread(Quantity, value = Value, fill = 0)
                in_table$Cooperative <- NULL
                in_table$Type <- NULL
                in_table$Meter <- NULL
                in_table$Quant_class <- NULL
                in_table$Vavg <- round(rowMeans(in_table[, c("Vab", "Vbc","Vca")]), 2)
                return(in_table)
            }
        },
        filter = "bottom",
        selection = "none",
        options = list(pageLength = 10, columnDefs = list(list(className = 'dt-center', targets = "_all")))
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
        output$dens_Vphase <- renderPlot({
            if (nrow(Vphase_Data()) < 2){
                return (NULL)
            }
            else {
                in_data <- Vphase_Data()
                plot_data <- in_data[in_data$Value >= input$Box_minVphase,]
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
        output$box_Vphase <- renderPlot({
            if (nrow(Vphase_Data()) < 2){
                return (NULL)
            }
            else {
                plot_data <- Vphase_Data() %>% filter (Value >= as.numeric(input$Box_minVphase))
                if (nrow(plot_data) <10){
                    return(NULL)
                }
                else{
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


      # output$Box_minVline_t <- renderUI({
    #     in_data <- Vline_Data()$Value
    #     if (length(in_data) > 2){
    #         min_val = plyr::round_any(min(in_data), 100, f = floor)
    #         max_val = plyr::round_any((min_val +(max(in_data) - min_val)*0.95), 100,  f = floor)
    #         sliderInput("Box_minVline",
    #                     label = "Voltaje Minimo (Gráficos)",
    #                     min = min_val,
    #                     max = max_val,
    #                     value = min_val)
    #     }
    #     else{
    #         NULL
    #     }
    # })     

       

