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
                selectizeInput(inputId = "alimento",
                               label = "Selecione um ou mais alimentos",
                               choices = "Carregando...",
                               selected = NULL,
                               multiple = TRUE)
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
            plotOutput("serie_ipca", height = "200px") # controla a altura do box
            ),
          box(
            width = 6,
            title = "Correlação - 2007 a 2011",
            solidHeader = TRUE, 
            status = "primary",  
            plotOutput("corrplot_1")
          ),
          box(
            width = 6,
            title = "Correlação - 2012 a 2020",
            solidHeader = TRUE, 
            status = "primary",  
            plotOutput("corrplot_2")
          )
        ),
        fluidRow(
          box(
            width = 12,
            title = "Estatística Descritiva",
            solidHeader = TRUE, 
            status = "primary",  
            tableOutput("estadesc")
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
      choices = alimentos,
      selected = alimentos[1]
    )
  })
  
  output$serie_ipca <- renderPlot({
    ipca |> 
      filter(Cadeia == input$cadeia) |> 
      filter(Alimento %in% input$alimento) |> 
      filter(Process. %in% input$processo) |> 
      ggplot(aes(x=data, y=var_ipca, color=Alimento)) +
      geom_line()+
      theme_minimal()+
      theme(legend.position = "bottom")
  })
  
  output$corrplot_1 <- renderPlot({
    ipca |> filter(Cadeia == input$cadeia,data <= as.Date("2011-12-31")) |> 
      tidyr::drop_na() |> 
      tidyr::pivot_wider(id_cols = data,
                         names_from =Alimento,
                         values_from = var_ipca
                        ) |> 
      select(-data) |> 
      cor() |> 
      corrplot::corrplot(method = "ellipse", type = "upper")
  })
  
  output$corrplot_2 <- renderPlot({
    ipca |> filter(Cadeia == input$cadeia,data > as.Date("2011-12-31")) |>
      tidyr::drop_na() |> 
      tidyr::pivot_wider(id_cols = data,
                         names_from =Alimento,
                         values_from = var_ipca) |> 
      select(-data) |> 
      cor() |> 
      corrplot::corrplot(method = "ellipse", type = "upper")
  })
  
  output$estadesc <- renderTable({
      est<-function(x) {
        na=sum(is.na(x))
        x <-na.omit(x)
        c(media=mean(x),
          dp=sd(x),
          Na = na,
          minimo = min(x),
          mediana = median(x),
          maximo = max(x),
          assimetria = agricolae::skewness(x),
          curtose = agricolae::kurtosis(x))
      }
    
      tab <- ipca |> filter(Cadeia == input$cadeia) |>
      tidyr::pivot_wider(id_cols = data,
                         names_from =Alimento,
                         values_from = var_ipca) |> 
      select(-data) |> 
      summarise(
        across(.cols = is.numeric,
               .fns = est)) |>
      t() 
      
      colnames(tab) <- c("Média","Desv_Pad","Nas","Mínimo","Mediana","Maximo",
                         "Assimetria","Curtose")
      nomes_r <- row.names(tab)
      tab <- as.tibble(tab)
      tab$Alimento <- nomes_r
      tab |> 
        relocate(Alimento)
  })
}

shinyApp(ui, server)