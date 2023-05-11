library(ggplot2)
library(shiny)
source("utils.R")
source("calculadora.R")


# define a interface UI
ui <- navbarPage(
  title = "Análise Financeira",
  
  tabPanel("Viabilidade do Projetos",
           sidebarLayout(
             sidebarPanel(
               h3("Dados de entrada"),
               numericInput("tx_juros", label = "Taxa de juros (%)", value = 10),
               textInput("fluxo_caixa", label = "Fluxo de caixa (separado por vírgulas)", value = "-100,-100,100,100,200"),
               actionButton("calcular", "Calcular")
             ),
             mainPanel(
               tabsetPanel(
                 tabPanel("Resultados",
                          h3("Resultados da análise"),
                          tableOutput("tabela_resultados")
                 )
               )
             )
           )
  ),
  
  tabPanel("Juros Simples",
           fluidPage(
             h2("Juros Simples Content"),
             # Add content here
           )
  ),
  
  tabPanel("Juros Composto",
           fluidPage(
             h2("Juros Composto Content"),
             # Add content here
           )
  ),
  tabPanel("Estatística",
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
  
}


# run  app
shinyApp(ui = ui, server = server)
