library(shiny)
library(plotly)
library(shinydashboard)
source("Funciones.R")

# Define UI for application that draws a histogram
ui <- dashboardPage(skin = "yellow",
                    dashboardHeader(title = "Fix Flooring Solutions"),
                    dashboardSidebar(
                      sidebarMenu(
                        menuItem("Programación", tabName = "Prog", icon = icon("calendar-alt")),
                        fileInput("inputMaq","Cargar archivo de grupos",
                                  accept = c("text/csv","text/comma-separated-values,
                                       text/plain",".csv",".xlsx")),
                        fileInput("inputTrab","Cargar archivo de trabajos",
                                  accept = c("text/csv","text/comma-separated-values,
                                       text/plain",".csv",".xlsx")),
                        conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                                         tags$div(h4("Loading..."),id="loadmessage")),
                        actionButton("guardar", "Guardar cambios")
                      )
                    ),
                    dashboardBody(
                      tabItems(
                        tabItem(tabName = "Prog",
                                fluidRow(
                                  box(title = "Gantt", status = "info", collapsible = F, collapsed = F,
                                      plotlyOutput("Gantt"), width = 12),
                            
                                  box(title = "Trabajos", tableOutput("trabajosTabla")),
                                  box(title = "Grupos", tableOutput("gruposTabla")),
                                  
                                  valueBoxOutput("Lmax", width = 6),
                                  valueBoxOutput("Tardios", width = 6)
                                )
                        )
                        #cierra el objeto tab items  
                      )
                      #cierra el dashboard body  
                    )
) 

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  ArchivoMaq <- eventReactive(input$guardar, {
    #Importar el archivo de máquinas
    inFile <- input$inputMaq
    if(is.null(inFile)) {return(NULL)}
    dataFile <- read.csv(inFile$datapath)
    return(dataFile)
  })
  
  ArchivoTrab <- eventReactive(input$guardar, {
    #Importar el archivo de trabajos
    inFile <- input$inputTrab
    if(is.null(inFile)) {return(NULL)}
    dataFile <- read.csv(inFile$datapath)
    return(dataFile)
  })
  
  Programacion <- eventReactive(input$guardar, {
    maquinas <- ArchivoMaq()
    trabajos <- ArchivoTrab()
    
    sol <- LRPT(trabajos, maquinas)
    return(sol)
  })
  
  output$Lmax <- renderValueBox({
    sol <- Programacion()
    
    val <- "--"
    if(!is.null(sol)){
      val <- sol$Lmax
    }
    valueBox(val, "Lmax", icon = icon("bullseye"), color = "green")
  })
  
  output$Tardios <- renderValueBox({
    sol <- Programacion()
    
    val <- "--"
    if(!is.null(sol)){
      val <- length(which(sol$trabajos$Lj > 0))
    }
    valueBox(val, "Trabajos tardíos", icon = icon("user-times"), color = "red")
  })
  
  output$Gantt <- renderPlotly({
    
    sol <- Programacion()
    
    if(!is.null(sol)){
      gantt(sol)
    }
  })
  
  output$trabajosTabla <- renderTable({
    
    sol <- Programacion()
    
    if(!is.null(sol)){
      sol$trabajos[c("Trabajo", "Tipo", "Apartamentos",
                 "pj", "dj", "Fin", "Lj")]
    }
  })
  
  output$gruposTabla <- renderTable({
    
    sol <- Programacion()
    
    if(!is.null(sol)){
      sol$grupos
    }
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
