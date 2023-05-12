library(ggplot2)
library(shiny)
source("utils.R")
source("calculadora.R")


# define a interface UI
ui <- navbarPage(
  title = "Análise Financeira",
  
  tabPanel("Viabilidade do Projetos",
           
           fluidPage(
           sidebarLayout(
             sidebarPanel(
               h3("Dados de entrada"),
               numericInput("tx_juros", label = "Taxa de juros (%)", value = 10),
               textInput("fluxo_caixa", label = "Fluxo de caixa (separado por vírgulas)", value = "-100,-100,100,100,200"),
               actionButton("calcular", "Calcular")
             ),
             
             mainPanel(
               tabsetPanel(
                 
                 tabPanel("Investimentos",
                          h2("Resultados da Análise"),
                          tableOutput("tabela_resultados"),
                          plotOutput("grafico_fluxo_caixa")
                 ),
                 
                 tabPanel("Taxa de Juros",
                          h2("Métricas em Funçao da Taxa de Juros"),
                          tableOutput("tabela_resultados_juros"),
                          plotOutput("grafico_vpl"),
                          plotOutput("grafico_roi"),
                          plotOutput("grafico_payback_simples"),
                          plotOutput("grafico_payback_descontado")
                 ),
            
               )
             )
           )
        )
  ),
  
  tabPanel("Juros Simples",
           
           fluidPage(
             # Add content here
             sidebarLayout(
               sidebarPanel(
                 h3("Dados de entrada"),
                 numericInput("capital", "Capital Inicial:", value = 1000, min = 0),
                 numericInput("taxa", "Taxa de Juros (em %):", value = 5, min = 0),
                 numericInput("tempo", "Tempo (em anos):", value = 1, min = 0),
                 actionButton("calcular3", "Calcular")
               ),
               mainPanel(
                 tabsetPanel(
                   tabPanel("Resultados",
                            
                            textOutput("montante")
                   )
                 )
                 
               )
             )
          )
  ),
  
  tabPanel("Juros Composto",
           fluidPage(
             h2("Juros Composto Content"),
             # Add content here
           )
  ),
  tabPanel("Estatística",
           
          fluidPage(
           sidebarLayout(
             sidebarPanel(
               h3("Dados de entrada"),
               numericInput("populacao", label = "População", value = 210000000),
               selectInput("grau_confianca", label = "Grau de Confiança (%)", choices = c("99", "95", "90", "85", "Outro")),
               numericInput("margem_erro", label = "Margem de Erro (%)", value = 1),
               numericInput("proporcao", label = "Proporção (%)", value = 50),
               actionButton("calcular2", "Calcular")
             ),
             mainPanel(
               tabsetPanel(
                 tabPanel("Resultados",
                          h3("Tamanho da Amostra"),
                          textOutput("tamanho_amostra")
                 )
               )
             )
           )
          )
  )
)



# define o servidor
server <- function(input, output) {
  

  
  # reage ao botão calcular
  observeEvent(input$calcular, {
    
    #trata o input
    fluxo_caixa <- string_para_vetor(input$fluxo_caixa)
    tx_juros <- input$tx_juros/100
    
    #trata o output
    
    #analise de investimento
    output$tabela_resultados <- renderTable({ gerar_tabela_investimento(fluxo_caixa, tx_juros) })
    output$grafico_fluxo_caixa <- renderPlot({grafico_fluxo_caixa(fluxo_caixa)})
    
    
    #analise de taxa de juros
    tabela_func_juros <-  gerar_tabela_juros(fluxo_caixa)
    output$tabela_resultados_juros <- renderTable({tabela_func_juros })
    output$grafico_vpl <- renderPlot({grafico_juros(tabela_func_juros,"vpl",tx_juros*100)})
    output$grafico_roi <- renderPlot({grafico_juros(tabela_func_juros,"roi",tx_juros*100)})
    output$grafico_payback_simples <- renderPlot({grafico_juros(tabela_func_juros,"payback_simples",tx_juros*100)})
    output$grafico_payback_descontado <- renderPlot({grafico_juros(tabela_func_juros,"payback_descontado",tx_juros*100)})
    
  })
  
  #Botão de Calcular de "Estatísticas"
  observeEvent(input$calcular2, {
    
    #trata o input
    populacao <- input$populacao
    confianca <- as.numeric(input$grau_confianca)/100
    margem_erro <- as.numeric(input$margem_erro)/100
    proporcao <- as.numeric(input$proporcao)/100
    
    #analise estatistica: tamanho de amostra
    output$tamanho_amostra <- renderText({
      tamanho_amostra <- calcular_tamanho_amostra(populacao, confianca, margem_erro, proporcao)
      paste(tamanho_amostra)})
      #paste("Tamanho da amostra: ",tamanho_amostra)})
    
  })
  
  # Botão de calcular juros simples
  observeEvent(input$calcular3, {
    capital <- input$capital
    taxa <- input$taxa / 100
    tempo <- input$tempo
    juros <- capital * taxa * tempo
    montante <- capital + juros
    
    if (tempo == 1){
      output$montante <- renderText({
        paste0("O montante após ", tempo, " ano será de R$ ", round(montante, 2))
      })
    }
    else {
      output$montante <- renderText({
        paste0("O montante após ", tempo, " anos será de R$ ", round(montante, 2))
      })
    }
  })
  
}


# run  app
shinyApp(ui = ui, server = server)
