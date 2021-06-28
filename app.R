library(tidyverse)
library(shinydashboard)
library(shiny)
library(vegan)
library(ggdendro)
library(plotly)

ipca <- readr::read_rds("data/ipca.rds")

ui <- dashboardPage(skin = "green",
  header = dashboardHeader(title = "Preço de Alimentos"),
  sidebar = dashboardSidebar(
    sidebarMenu(
      menuItem("Visualização",tabName = "vasualizacao", 
               icon = icon("eye")),
      menuItem("Análise Multivariada",tabName = "multivariada", 
               icon = icon("cogs"))
    )
  ),
  body = dashboardBody(
    tags$link(
      rel = "stylesheet",
      type = "text/css",
      href = "custom.css"
    ),
    tabItems(
# ui visualização ---------------------------------------------------------
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
            plotOutput("serie_ipca") # controla a altura do box
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
      ),

#ui multivariada ------------------------------------------------------------
      tabItem(
        tabName = "multivariada",
        fluidRow(
          column(
            width = 12,
            h1("Análise Multivariada dos dados")
          )
        ), 
        hr(style = "border-top: 1px solid black;"), 
        br(),
        fluidRow(
          box(
            width = 6,
            title = "Seleção da variáveis",
            solidHeader = TRUE,
            status = "primary",
            fluidRow(
              column(
                width = 6,
                selectizeInput(
                  inputId = "lista_multivaria",
                  label = "Selecione um ou mais grupos",
                  choices = ipca |> 
                    filter(!is.na(Cadeia)) |> 
                    pull(Cadeia) |>
                    unique(),
                  selected = c("Arroz","Milho","Complexo soja","Café"),
                  multiple = TRUE,
                  options = NULL
                )
              ),
              fluidRow(
                column(
                  width = 6,
                  checkboxGroupInput(
                    inputId = "processo_mult",
                    label = "Tipo de Processamento",
                    choices = c("G1","G2","G3","G4"),
                    selected = c("G1","G2","G3","G4"),
                    inline = TRUE
                  )
                )
              )
            ),
            fluidRow(
                column(
                  width = 12,
                  selectizeInput(
                    inputId = "alimento_mult",
                    label = "Selecione os alimentos",
                    choices = "Carregando...",
                    selected = NULL,
                    multiple = TRUE
                )
              )
            )
          ),
          box( 
            width = 6,
            height = "220px",
            title = "Selecione o período",
            solidHeader = TRUE,
            status = "primary",
            fluidRow(
              column(
                width = 12,
                dateRangeInput(
                  'periodo_datas',
                  label = "",
                  start = min(ipca$data), end = max(ipca$data),
                  # min = Sys.Date() - 10, max = Sys.Date() + 10,
                  separator = " até ", format = "dd/mm/yy",
                  startview = "year",
                )
              )
            )
          )
        ),
        fluidRow(
          box(
            width = 6,
            title = "Biplot",
            solidHeader = TRUE,
            status = "primary",
            fluidRow(
              column(
                width = 12,
                plotOutput("biplot")
              )
            )
          ),
          box(
            width = 6,
            title = "Dendrograma",
            solidHeader = TRUE,
            status = "primary",
            fluidRow(
              column(
                width = 12,
                plotlyOutput("dendrograma")
              )
            )
          )
        )
      )
    )
  ),
title = "Ipca Preço Alimentos"
)

server <- function(input, output, session) {
  ipca <- readr::read_rds("data/ipca.rds")

#server visualização -------------------------------------------------------------

  
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
    est<-function(x){
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
  
  
  # server - multivariada ---------------------------------------------------
  base_multivariada <- reactive({
    req(input$lista_multivaria,input$processo_mult)
    ipca |> 
      filter(Cadeia %in%  input$lista_multivaria) |> 
      filter(Process. %in% input$processo_mult) |> 
      filter(data >= input$periodo_datas[1] & data<=input$periodo_datas[2])
  })
  
  observe({
    alimentos <- base_multivariada() |> 
      pull(Alimento) |> unique()
    
    updateSelectizeInput(
      session,
      "alimento_mult",
      choices = c("Todos",alimentos),
      selected = "Todos"
    )
    
  })
  
  base_multivariada_alimento <- reactive({
    req(input$alimento_mult)
    if(input$alimento_mult[1] == "Todos"){
      base_multivariada() |> 
        mutate(ano = lubridate::year(data),
               mes = lubridate::month(data)) |> 
        select(Alimento,ano,mes,data,var_ipca) |> 
        pivot_wider(
          names_from = "Alimento",
          values_from = "var_ipca"
        )
    } else {
      base_multivariada() |> 
        mutate(ano = lubridate::year(data),
               mes = lubridate::month(data)) |> 
        filter(Alimento %in%  input$alimento_mult) |> 
        select(Alimento,ano,mes,data,var_ipca) |> 
        pivot_wider(
          names_from = "Alimento",
          values_from = "var_ipca"
        )
    }
  })
  
  da <- reactive({
    base_multivariada_alimento() |> 
      drop_na()
  })
  
  output$tabela_multivariada <- renderTable({
    pca <- prcomp(da() |> 
                   select(-data, -ano, -mes), scale.=T)
    mcor <- cor(da()|> 
                   select(-data, -ano, -mes), pca$x)
    nomes_pcs <- colnames(mcor)
    mcor <- t(mcor)
    mcor <- as_tibble(mcor)
    mcor$PC <- nomes_pcs

    tab <- tibble(
      Autovalor=pca$sdev^2,
      Var_exp=Autovalor/sum(Autovalor),
      Var_exp_acum=cumsum(Var_exp)*100,
      mcor
    ) |> relocate(PC)

    tab
  })
  
  output$biplot<- renderPlot({
    rotulos <- da() |> 
      pull(ano)
    my_biplot(da() |> 
                select(-data, -ano, -mes),
              rotulos,"")
  })
  
  output$dendrograma<- renderPlotly({
    rotulos <- da() |>
      pull(ano)
    # browser()
    da <- da()
    da <- da |> 
         mutate(rt=paste(ano,mes,sep="_")) 
    
    da <-  column_to_rownames(da, var = "rt")
    da_pad<- decostand(da |>
                          select(-data,-ano, -mes),
                        method = "standardize",na.rm=TRUE)
    da_pad_euc<-vegdist(da_pad,"euclidean")
    da_pad_euc_ward<-hclust(da_pad_euc, method="ward.D")
     # plot(da_pad_euc_ward,ylab="Distância Euclidiana",xlab="Acessos",
     #      hang=-1,col="blue",las=1,cex=.6,labels = rotulos,lwd=1.5);box()
    ggdendrogram(da_pad_euc_ward)
  })
  
}

shinyApp(ui, server)

