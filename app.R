library(tidyverse)
library(shinydashboard)
library(shiny)

ipca <- readr::read_rds("data/ipca.rds")

ui <- dashboardPage(
  header = dashboardHeader(title = "Preço de Alimentos"),
  sidebar = dashboardSidebar(
    sidebarMenu(
      menuItem("Visualização",tabName = "vasualizacao")
    )
  ),
  body = dashboardBody(
    tabItems(
      tabItem(
        tabName = "vasualizacao",
        fluidRow(
          column(
            width = 12,
            h1("Visualização da Variação do IPCA")
          )
        ), 
        hr(style = "border-top: 1px solid black;"), 
        br(),
        fluidRow(
          box(
            width = 12,
            fluidRow(
              column(
                width = 4,
                selectInput(
                  inputId = "cadeia",
                  label = "Selecione a cadeia",
                  choices = sort(unique(ipca$Cadeia)),
                  selected = "Arroz"
                )
              ),
              column(
                width = 4,
                # selectInput(
                #   inputId = "alimento",
                #   label = "Selecione o alimento",
                #   choices = "Carregando..."
                # )
                selectizeInput(inputId = "alimento",
                               label = "Selecione um ou mais alimentos",
                               choices = "Carregando...",
                               selected = NULL,
                               multiple = TRUE,
                               options = NULL)
              ),
              column(
                width = 4,
                checkboxGroupInput(inputId = "processo",
                                   label = "Tipo de Processamento",
                                   choices = c("G1","G2","G3","G4"),
                                   selected = c("G1","G2","G3","G4"))
              )
            )
          )
        ),
        fluidRow(
          box(
            width = 12,
            title = "Série Temporal - Variação do IPCA",
            solidHeader = TRUE, 
            status = "primary",  
            plotOutput("serie_ipca") #, height = "200px") # controla a altura do box
          )
        )
      )
    )
  ),
  title = "Ipca Preço Alimentos"
)

server <- function(input, output, session) {
  ipca <- readr::read_rds("data/ipca.rds")
  
  observe({
    cadeias <- ipca |> 
      pull(Cadeia) |> 
      unique() |> 
      sort()
    updateSelectInput(
      session,
      "cadeia",
      choices = cadeias
    )
  })
  
  observe({
    alimentos <- ipca |> 
      filter(Cadeia == input$cadeia) |> 
      pull(Alimento) |> 
      unique() |> 
      sort()
      
    updateSelectInput(
      session,
      "alimento",
      choices = c("Todos",alimentos),
      selected = "Todos"
    )
  })
  
  output$serie_ipca <- renderPlot({
    if(is.null(input$alimento)) {
      Ali = ""
    } else{
      Ali = input$alimento
    }
    
    ipca |> 
      # browser() |> 
      filter( Cadeia == input$cadeia) |> 
      # filter(Ali %in% Alimento) |> 
      ggplot(aes(x=data, y=var_ipca, color=Ali)) +
      geom_line()+
      theme_minimal()
  })
  
}

shinyApp(ui, server)