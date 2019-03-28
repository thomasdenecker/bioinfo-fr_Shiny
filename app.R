################################################################################
# Bioinfo-fr.net - Shiny
# Thomas DENECKER
# 04 /2019
#
# GitHub :
# 
################################################################################

################################################################################
# Library
################################################################################

library(shiny)
library(shinydashboard)
library(DT)
library(plotly)
library(ggplot2)
library(shinyWidgets)
library(colourpicker)
library(shinyjs)
library(shinycssloaders)

################################################################################
# UI
################################################################################

ui <- dashboardPage(
  dashboardHeader(title = "bioinfo-fr"),
  dashboardSidebar(
    sidebarMenu(id = "tabs",
      menuItem("Lecture des données", tabName = "readData", icon = icon("readme")),
      menuItem("Visualisation des données", tabName = "visualization", icon = icon("poll"))
    )
  ),
  dashboardBody(
    tags$head(tags$style(HTML('
      .content-wrapper{
        background-color: white !important;
      }
    '))),
              
    tabItems(
      # Read data
      tabItem(tabName = "readData",
              h1("Lecture des données"),
              fileInput("dataFile",label = NULL,
                        buttonLabel = "Browse...",
                        placeholder = "No file selected"),
              
              fluidRow(
                column(3,
                       h3("Parameters"),
                       
                       # Input: Checkbox if file has header
                       radioButtons(inputId = "header", 
                                    label = "Header",
                                    choices = c("Yes" = TRUE,
                                                "No" = FALSE),
                                    selected = TRUE, inline=T),
                       
                       # Input: Select separator ----
                       radioButtons(inputId = "sep", 
                                    label = "Separator",
                                    choices = c(Comma = ",",
                                                Semicolon = ";",
                                                Tab = "\t"),
                                    selected = "\t", inline=T),
                       
                       # Input: Select quotes ----
                       radioButtons(inputId = "quote", 
                                    label= "Quote",
                                    choices = c(None = "",
                                                "Double Quote" = '"',
                                                "Single Quote" = "'"),
                                    selected = "", inline=T)
                ),
                column(9,
                       h3("File preview"),
                       dataTableOutput(outputId = "preview")
                )
              ), 
              tags$br(),
              
              div(actionButton(inputId = "actBtnVisualisation", label = "Visualisation",icon = icon("play") ), align = "center")
              
              
              
      ),
      
      # visualization
      tabItem(tabName = "visualization",
              h1("Visualisation des données"),
              h2("Exploration du tableau"),
              dataTableOutput('dataTable'),
              h2("Graphiques"),
              fluidRow(
                column(3, plotOutput("plotAvecR")),
                column(3, plotOutput("plotAvecGgplot2")),
                column(3, plotlyOutput("plotAvecPlotly")),
                column(3, htmlOutput("plotAvecGoogle"))
              )
              
      )
    )
  )
)

################################################################################
# Server
################################################################################

server <- function(input, output, session) {
  
  data = reactiveValues()
  
  #=============================================================================
  # Preview
  #=============================================================================
  output$preview <-  renderDataTable({
    
    req(input$dataFile)
    
    df <- read.csv(input$dataFile$datapath,
                   header = as.logical(input$header),
                   sep = input$sep,
                   quote = input$quote,
                   nrows=10
    )
  },  options = list(scrollX = TRUE , dom = 't'))
  
  #=============================================================================
  # Lecture
  #=============================================================================
  observeEvent(input$actBtnVisualisation, {
    
    if(!is.null(input$dataFile$datapath)){
      data$table = read.csv(input$dataFile$datapath,
                            header = as.logical(input$header),
                            sep = input$sep,
                            quote = input$quote)
      sendSweetAlert(
        session = session,
        title = "Done !",
        text = "Le fichier a bien été lu !",
        type = "success"
      )
      
      updateTabItems(session, "tabs", selected = "visualization")
    }
    
  })
  
  #=============================================================================
  # Exploration du tableau
  #=============================================================================
  
  output$dataTable = DT::renderDataTable({
    datatable(data$table, filter = 'top') %>% 
      formatStyle('Sepal.Length', 
                  background = styleColorBar(data$table$Sepal.Length, 'lightcoral'),
                  backgroundSize = '100% 90%',
                  backgroundRepeat = 'no-repeat',
                  backgroundPosition = 'center'
      ) %>%
      formatStyle(
        'Sepal.Width',
        backgroundColor = styleInterval(c(3,4), c('white', 'red', "firebrick")),
        color = styleInterval(c(3,4), c('black', 'white', "white"))
      ) %>%
      formatStyle(
        'Petal.Length',
        background = styleColorBar(data$table$Petal.Length, 'lightcoral'),
        backgroundSize = '100% 90%',
        backgroundRepeat = 'no-repeat',
        backgroundPosition = 'center'
      ) %>%
      formatStyle(
        'Petal.Width',
        backgroundColor = styleInterval(c(1,2), c('white', 'red', "firebrick")),
        color = styleInterval(c(1,2), c('black', 'white', "white"))
      ) %>%
      formatStyle(
        'Species',
        backgroundColor = styleEqual(
          unique(data$table$Species), c('lightblue', 'lightgreen', 'lavender')
        )
      )
    
  })
  
  output$plotAvecR <- renderPlot({
    plot(data$table$Petal.Length,data$table$Sepal.Length, 
         main = "Sepal length vs Petal length (R)",
         ylab = "Sepal length",
         xlab = "Petal length")
  })
  
  output$plotAvecGgplot2 <- renderPlot({
    ggplot(data=data$table, aes(x = Sepal.Length, y = Sepal.Width)) + 
      geom_point(aes(color=Species, shape=Species)) +
      xlab("Sepal Length") +  ylab("Sepal Width") +
      ggtitle("Sepal Length-Width (ggplot2)")
  })
  
  output$plotAvecPlotly <- renderPlotly({
    if(!is.null(data$table)){
      plot_ly(data = data$table, x = ~Petal.Length, y = ~Petal.Width, color = ~Species)%>%
        layout(title = 'Petal Length-Width (ggplot2)',
               yaxis = list(title = "Petal width"),
               xaxis = list(title = "Petal length"))
    }else {
      NULL
    }

  })
  
  output$plotAvecGoogle <- renderGvis({
    if(!is.null(data$table)){
      gvisHistogram(as.data.frame(data$table$Petal.Width),
                    options=list(title ="Petal width (Google)",
                                 height=400)
                    )
    }else {
      NULL
    }
    
  })
  
  
}

shinyApp(ui, server)
