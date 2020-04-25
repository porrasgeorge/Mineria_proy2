library(shiny)
##library(shinyFiles)
library(lubridate)
library(ggplot2)
library(dplyr) 
library(tidyr)
library(scales)
library(openxlsx)
source("DB.R")
source("methods.R")

end_dateCR <- floor_date(now(), "week")
start_dateCR <- end_dateCR - weeks(1)+ days(1)

tn <- 24900
tensiones <- c(tn, 0.87*tn, 0.91*tn,0.93*tn,0.95*tn,1.05*tn,1.07*tn,1.09*tn,1.13*tn)
names(tensiones) <- c("Nom", "limit087" ,"limit091","limit093","limit095" ,"limit105" ,"limit107" ,"limit109" ,"limit113")

sources <- DBget_Sources()

ui <- fluidPage(
    titlePanel(title = h2("Coopeguanacaste", align = "center")),
    sidebarLayout(
        sidebarPanel(
            dateRangeInput("daterange", "Rango de Fecha",
                           start = start_dateCR,
                           end = end_dateCR,
                           ##min = min(Surveillance$Date),
                           ##max = max(Surveillance$Date),
                           separator = " - ", 
                           format = "dd/mm/yyyy",
                           startview = 'Week', 
                           language = 'es', 
                           weekstart = 1),  
            
            selectInput(inputId = 'selected_source',
                        label='Medidores',
                        choices=sources$Name),
            
            actionButton(inputId = "go", label = "Go"),
            submitButton("Aplicar"),
            
            downloadButton("downloadData", "Download"),
            
            selectInput("State", "Select One", c("Abc" = 1, "Cxa" = 2, "AAvvb" = 3), selected = 1),
            radioButtons("Staterb", "Select One", c("Abc" = 1, "Cxa" = 2, "AAvvb" = 3), selected = 2),
            p("Blah blah blah"),
            width = 2
            
        ),
        mainPanel(
            textOutput("message_text"),
            tabsetPanel(type = "tab", 
                        tabPanel("Summary" ),
                        tabPanel("Structure"),
                        tabPanel("Table", 
                                 h4("Tabla"),
                                 tableOutput('table')),
                        tabPanel("Plot", plotOutput("histogram"))
                        )

        )
    )
)


server <- function(input, output) {
    
###################################################################################### Reactivity
    selectedSourceId <- eventReactive(input$selected_source, {
    get_SourceIDbyName(sources, input$selected_source)
    })

    selected_sourceName_reactive <- eventReactive(input$go, {
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
        
        
###################################################################################### Table
        
        output$table <- renderTable(
            if (is.null(IonData_LineV())){
                return (NULL)
            }
            else {
                create_Percent_Table(IonData_LineV(),
                                     tensiones)
            }, 
            digits = 0, 
            striped = TRUE, 
            hover = TRUE, 
            bordered = TRUE)

###################################################################################### Plot
        output$histogram <- renderPlot({
            if (is.null(IonData_LineV())){
                return (NULL)
            }
            else {
                return(create_Histo_Plot(IonData_LineV(), 
                                         selected_sourceName_reactive()))
            }
        })

###################################################################################### downloads
        output$downloadData <- downloadHandler(
            filename = function() {
                paste(selected_sourceName_reactive(), ".xlsx", sep = "")
            },
            content = function(file) {
                saveWorkbook(create_Excel_file(create_Percent_Table(IonData_LineV(), 
                                                                    tensiones), 
                                               selected_sourceName_reactive()), 
                             file = file, 
                             overwrite = TRUE)
            }
            )
        

        }
shinyApp (ui = ui, server = server)