library(shiny)
library(lubridate)
library(ggplot2)
library(dplyr)
library(tidyr)
##library(openxlsx)
library(DT)
source("DB.R")
source("methods.R")

################################################################################################
## Declaracion de Variables

end_dateCR <- floor_date(now(), "week", week_start = 1)
start_dateCR <- end_dateCR - weeks(1)

#dataLog  <- read_feather("featherFiles/dataLog.feather")
dataLog  <- read_feather("featherFiles/dataLog_big2.feather")

coop_mets <- dataLog %>%
    distinct(Cooperative, Type, Meter, Quant_class, Quantity) %>%
    arrange(Cooperative, Type, Meter, Quant_class, Quantity)

################################################################################################

ui <- fluidPage(
    ##theme = shinytheme("united"),
    tags$head(
        HTML(
            "<title>CDC Conelectricas</title> <link rel='icon' type='image/gif/png' href='ConeLogo.jpg'>"
        )
    ),
    h4("Filtros:"),
    
    ####################################################################################################
    ## Menues
    fluidRow(
        column(
            3,
            dateRangeInput(
                "daterange",
                "Rango de Fecha",
                start = start_dateCR,
                end = end_dateCR,
                separator = " - ",
                format = "dd/mm/yyyy",
                startview = 'Week',
                language = 'es',
                weekstart = 1,
                width = "100%"
            )
        ),
        column(
            2,
            selectInput(
                inputId = 'coop_si',
                label = 'Cooperativa',
                choices = unique(coop_mets$Cooperative),
                width = "100%"
            )
        ),
        column(
            2,
            selectInput(
                inputId = 'type_si',
                label = 'Tipo',
                choices = "",
                selected = "",
                width = "100%"
            )
        ),
        column(
            2,
            selectInput(
                inputId = 'source_si',
                label = 'Medidor',
                choices = "",
                selected = "",
                width = "100%"
            )
        )
        
    ),
    ## fin de fluid row
    hr(),
    
    ####################################################################################################
    ## TABS
    tabsetPanel(
        type = "tab",
        
        ####################################################################################################
        ## Tab de tension de Linea
        tabPanel(
            "Vline",
            htmlOutput("Vline_NODATA"),
            tableOutput('Vline_table'),
            hr(),
            DTOutput('Voltline_DataTable'),
            hr(),
            column(
                3,
                
                #uiOutput(outputId = "Box_minVline"),
                sliderInput(
                    inputId = "Box_minVline",
                    label = "Voltaje Minimo (Gráficos)",
                    min = 0,
                    max = 35000,
                    value = 0
                )
            ),
            plotOutput("box_Vline"),
            hr(),
            plotOutput("dens_Vline")
        ),
        
        ####################################################################################################
        ## Tab de tension de Fase
        tabPanel(
            "Vphase",
            # br(),
            htmlOutput("Vphase_NODATA"),
            # textOutput("message_text_V"),
            tableOutput('Vphase_table'),
            hr(),
            DTOutput('Voltphase_DataTable'),
            hr(),
            column(
                3,
                sliderInput(
                    inputId = "Box_minVphase",
                    label = "Voltaje Minimo (Gráficos)",
                    min = 0,
                    max = 35000,
                    value = 0
                )
            ),
            plotOutput("box_Vphase"),
            hr(),
            plotOutput("dens_Vphase")
        ),
        
        ####################################################################################################
        ## Tab de Armonicos de Tension
        
        tabPanel("Armonicos de Tensión",
                 plotOutput("histogram"))
    ) ## tabsetPanel_2
    
) ## fluidpage


################################################################################################

server <- function(input, output, session) {
    ###################################################################################### Reactivity
    
    
    observeEvent(input$coop_si, {
        updateSelectInput(session,
                          "type_si",
                          label = 'Tipo',
                          choices = coop_mets$Type[coop_mets$Cooperative == input$coop_si])
        updateSelectInput(session,
                          "source_si",
                          label = 'Medidor',
                          choices = coop_mets$Meter[(coop_mets$Type == input$type_si) &
                                                        (coop_mets$Cooperative == input$coop_si)])
    })
    
    observeEvent(input$type_si, {
        updateSelectInput(session,
                          "source_si",
                          label = 'Medidor',
                          choices = coop_mets$Meter[(coop_mets$Type == input$type_si) &
                                                        (coop_mets$Cooperative == input$coop_si)])
    })
    #
    #     meter_data <- eventReactive(input$go, {
    #         return(filter_DataDataSelection(dataLog, input$source_si, input$daterange))
    #     })
    
    meter_data <- reactive({
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

    ####################################################################################################
    ## Calculo de tensiones nominales de linea y fase
    
    Vphase_Nominal <- reactive({
        return(guess_Nominal(Vphase_Data()$Value))
    })
    
    Vline_Nominal <- reactive({
        return(guess_Nominal(Vline_Data()$Value))
    })
    
    
    ####################################################################################################
    ## para las tablas de tensiones clasificadas del 90 al 110%
    
    Vphase_DataTable <- reactive({
        return(voltage_Summary(Vphase_Data(), Vphase_Nominal()))
    })

    Vline_DataTable <- reactive({
        return(voltage_Summary(Vline_Data(), Vline_Nominal()))
    })

    Vphase_DataTable_perc <- reactive({
        return(create_Percent_Table(Vphase_DataTable(), Vphase_Nominal()))
    })

    Vline_DataTable_perc <- reactive({
        return(create_Percent_Table(Vline_DataTable(), Vline_Nominal()))
    })
    
    ####################################################################################################
    ## Actualizar los Input Sliders
    
    observeEvent(Vphase_Data(), {
        print("meter_data va a ser modificada")
        in_data <- Vphase_Data()$Value
        if (length(in_data) > 2) {
            min_val = plyr::round_any(min(in_data), 100, f = floor)
            max_val = plyr::round_any((min_val + (max(
                in_data
            ) - min_val) * 0.95), 100,  f = floor)
            updateSliderInput(
                session,
                inputId = "Box_minVphase",
                label = "Voltaje Minimo (Gráficos)",
                min = min_val,
                max = max_val,
                value = min_val
            )
        }
        print("meter_data ha sido modificada")
    })
    
    observeEvent(Vline_Data(), {
        print("meter_data va a ser modificada")
        in_data <- Vline_Data()$Value
        if (length(in_data) > 2) {
            min_val = plyr::round_any(min(in_data), 100, f = floor)
            max_val = plyr::round_any((min_val + (max(
                in_data
            ) - min_val) * 0.95), 100,  f = floor)
            updateSliderInput(
                session,
                inputId = "Box_minVline",
                label = "Voltaje Minimo (Gráficos)",
                min = min_val,
                max = max_val,
                value = min_val
            )
        }
        print("meter_data ha sido modificada")
    })
    
    
    ####################################################################################################
    ## Textos para cantidad de datos
    
    output$Vphase_NODATA <- renderText({
        if (nrow(Vphase_Data()) < 2) {
            paste(
                "<br><font color=\"#FF0000\"><h2>No hay datos en el periodo seleccionado</h2></font>"
            )
        }
        else {
            paste(
                "<br>Se tienen: ",
                "<font color=\"#FF0000\"><b>",
                nrow(Vphase_Data()),
                "</b></font>",
                " datos"
            )
        }
    })
    
    output$Vline_NODATA <- renderText({
        if (nrow(Vline_Data()) < 2) {
            paste(
                "<br><font color=\"#FF0000\"><h2>No hay datos en el periodo seleccionado</h2></font>"
            )
        }
        else {
            paste(
                "<br>Se tienen: ",
                "<font color=\"#FF0000\"><b>",
                nrow(Vline_Data()),
                "</b></font>",
                " datos"
            )
        }
    })
    
    
    # output$message_text_V <- renderText({
    #     paste("Voltage nominal detectado: ", t_Nominal(), "V")
    # })
    
    
    ##########################################################################################
    ## TABLES
    
    ##########################################################################################
    ## Phase Voltage Table (Resumed and classified)
    output$Vphase_table <- renderTable(
        if (nrow(Vphase_Data()) < 2) {
            return (NULL)
        }
        else {
            Vphase_DataTable_perc()
            ##voltage_Summary(Vphase_Data(), Vphase_Nominal())
        },
        digits = 0,
        striped = TRUE,
        hover = TRUE,
        ##bordered = TRUE,
        width = '100%',
        align = 'c',
        caption = "<h1>Voltajes de Fase</h1>",
        caption.placement = "top"
    )
    
    ##########################################################################################
    ## Line Voltage Table (Resumed and classified)
    output$Vline_table <- renderTable(
        if (nrow(Vline_Data()) < 2) {
            return (NULL)
        }
        else {
            Vline_DataTable_perc()
            ##voltage_Summary(Vline_Data(), Vline_Nominal())
        },
        digits = 0,
        striped = TRUE,
        hover = TRUE,
        ##bordered = TRUE,
        width = '100%',
        align = 'c',
        caption = "<h1>Voltajes de Línea</h1>",
        caption.placement = "top"
    )
    
    
    ##########################################################################################
    ## Phase Voltage Table (Full Table)
    output$Voltphase_DataTable <- renderDT({
        if (nrow(Vphase_Data()) < 2) {
            shiny::showNotification("No data", type = "error")
            NULL
        }
        else {
            in_table <-
                Vphase_Data() %>% spread(Quantity, value = Value, fill = 0)
            in_table$Cooperative <- NULL
            in_table$Type <- NULL
            in_table$Meter <- NULL
            in_table$Quant_class <- NULL
            if (ncol(in_table) > 3){
                in_table$Vavg <-
                    round(rowMeans(in_table[, c("Van", "Vbn", "Vcn")]), 2)
            }
            return(in_table)
        }
    },
    filter = "bottom",
    selection = "none",
    caption = "Voltajes de Fase",
    options = list(pageLength = 10, columnDefs = list(
        list(className = 'dt-center', targets = "_all")
    )))
    
    
    ##########################################################################################
    ## Line Voltage Table (Full Table)
    output$Voltline_DataTable <- renderDT({
        if (nrow(Vline_Data()) < 2) {
            shiny::showNotification("No data", type = "error")
            NULL
        }
        else {
            in_table <-
                Vline_Data() %>% spread(Quantity, value = Value, fill = 0)
            in_table$Cooperative <- NULL
            in_table$Type <- NULL
            in_table$Meter <- NULL
            in_table$Quant_class <- NULL
            if (ncol(in_table) > 3){
                in_table$Vavg <-
                    round(rowMeans(in_table[, c("Vab", "Vbc", "Vca")]), 2)
            }
            return(in_table)
        }
    },
    filter = "bottom",
    selection = "none",
    caption = "Voltajes de Línea",
    options = list(pageLength = 10, columnDefs = list(
        list(className = 'dt-center', targets = "_all")
    )))
    
    ##########################################################################################
    ## PLOTS
    
    ##########################################################################################
    ## Voltage Predefined Histogram
    output$histogram <- renderPlot({
        if (nrow(Vphase_Data()) < 2) {
            return (NULL)
        }
        else {
            return(create_Histo_Plot(Vphase_Data(),
                                     input$source_si))
        }
    })
    
    ##########################################################################################
    ## Phase Voltage Density Plot
    output$dens_Vphase <- renderPlot({
        if (nrow(Vphase_Data()) < 2) {
            return (NULL)
        }
        else {
            in_data <- Vphase_Data()
            plot_data <-
                in_data[in_data$Value >= input$Box_minVphase, ]
            lineas <-
                plot_data %>% group_by(Quantity) %>% summarise(v = mean(Value))
            if (nrow(plot_data) < 10) {
                return(NULL)
            }
            else{
                hplot <- ggplot(plot_data, aes(x = Value, fill = Quantity)) +
                    geom_density(alpha = 0.6) +
                    geom_vline(
                        data = lineas,
                        aes(xintercept = v, color = Quantity),
                        size = 2
                    )
                
                return(hplot)
            }
        }
    })
    
    ##########################################################################################
    ## Line Voltage Density Plot
    output$dens_Vline <- renderPlot({
        if (nrow(Vline_Data()) < 2) {
            return (NULL)
        }
        else {
            in_data <- Vline_Data()
            plot_data <-
                in_data[in_data$Value >= input$Box_minVline, ]
            lineas <-
                plot_data %>% group_by(Quantity) %>% summarise(v = mean(Value))
            if (nrow(plot_data) < 10) {
                return(NULL)
            }
            else{
                hplot <- ggplot(plot_data, aes(x = Value, fill = Quantity)) +
                    geom_density(alpha = 0.6) +
                    geom_vline(
                        data = lineas,
                        aes(xintercept = v, color = Quantity),
                        size = 2
                    )
                
                return(hplot)
            }
        }
    })
    
    ##########################################################################################
    ## Phase Voltage Box Plot
    output$box_Vphase <- renderPlot({
        if (nrow(Vphase_Data()) < 2) {
            return (NULL)
        }
        else {
            plot_data <-
                Vphase_Data() %>% filter (Value >= as.numeric(input$Box_minVphase))
            if (nrow(plot_data) < 10) {
                return(NULL)
            }
            else{
                bp <- ggplot(plot_data, aes(Quantity, Value)) +
                    geom_boxplot(aes(colour = Quantity))
                return(bp)
            }
        }
    })
    ##########################################################################################
    ## Line Voltage Box Plot
    output$box_Vline <- renderPlot({
        if (nrow(Vline_Data()) < 2) {
            return (NULL)
        }
        else {
            plot_data <-
                Vline_Data() %>% filter (Value >= as.numeric(input$Box_minVline))
            if (nrow(plot_data) < 10) {
                return(NULL)
            }
            else{
                bp <- ggplot(plot_data, aes(Quantity, Value)) +
                    geom_boxplot(aes(colour = Quantity))
                return(bp)
            }
        }
    })
    
    
}
shinyApp (ui = ui, server = server)