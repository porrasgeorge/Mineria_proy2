library(shiny) 
library(lubridate)
library(ggplot2)
library(dplyr) 
library(tidyr)
library(scales)
source("DB.R")
source("methods.R")

end_dateCR <- floor_date(now(), "week")
start_dateCR <- end_dateCR - weeks(1)+ days(1)

tn <- 24900
tensiones <- c(tn, 0.87*tn, 0.91*tn,0.93*tn,0.95*tn,1.05*tn,1.07*tn,1.09*tn,1.13*tn)
names(tensiones) <- c("Nom", "limit087" ,"limit091","limit093","limit095" ,"limit105" ,"limit107" ,"limit109" ,"limit113")

sources <- DBget_Sources()

ui <- fluidPage(
    titlePanel("Coopeguanacaste"),
    sidebarLayout(
        sidebarPanel(
            dateRangeInput("daterange", "Rango de Fecha",
                           start = start_dateCR,
                           end = end_dateCR,
                           ##min = min(Surveillance$Date),
                           ##max = max(Surveillance$Date),
                           separator = " - ", format = "dd/mm/yyyy",
                           startview = 'Week', language = 'es', weekstart = 1),  
            
            selectInput(inputId = 'selected_source',
                      label='Medidores',
                      choices=sources$Name),
            
            actionButton(inputId = "go", label = "Go"),
            
            downloadButton("downloadData", "Download")
            
        ),
        mainPanel(
            textOutput("message_text"),
            tableOutput('table'),
            plotOutput("histogram")
        )
    )
)

server <- function(input, output) {
    selectedSourceId <- eventReactive(input$selected_source, {
        get_SourceIDbyName(sources, input$selected_source)
        })
    
    selected_sourceName_reactive <- eventReactive(input$selected_source, {
        paste0(input$selected_source)
    })
    
    IonData_LineV <- eventReactive(input$go, {
        DBget_DataLineVoltage(sources, selectedSourceId(), input$daterange, tensiones)
        })

        output$message_text <- renderText({ 
            if (is.null(IonData_LineV())){
                paste("No hay datos en el periodo seleccionado")
            }
            else {
                paste("Se tiene: ", sum(IonData_LineV()$Freq), " datos")
            }
        })
        
        output$table <- renderTable(
            if (is.null(IonData_LineV())){
                return (NULL)
            }
            else {
                create_Percent_Table(IonData_LineV(), tensiones)
            })
        
        output$histogram <- renderPlot({
            if (is.null(IonData_LineV())){
                return (NULL)
            }
            else {
            ggplot(IonData_LineV(), aes(x = Classif, y = Perc, label = Perc, fill = Quantity )) +
                geom_col(position = "dodge", width = 0.7) +
                scale_y_continuous(labels = function(x) paste0(100*x, "%"), limits = c(0, 1.2)) +
                geom_text(aes(label=sprintf("%0.2f%%", 100*Perc)), 
                          position = position_dodge(0.9), 
                          angle = 90, size = 2) + 
                theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 8)) +
                ggtitle(input$selected_source) +
                xlab("Medicion") + 
                ylab("Porcentaje")
            }
        })
        
        output$downloadData <- downloadHandler(
            filename = function() {
                selected_sourceName_reactive()
            },
            content = function(file) {
                write.csv(create_Percent_Table(IonData_LineV(), tensiones), file, row.names = TRUE)
            }
        )
        
        
        }
shinyApp (ui = ui, server = server)