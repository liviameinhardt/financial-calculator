library(ggplot2)
library(shiny)
source("utils.R")
source("calculadora.R")


# define a interface UI
ui <- fluidPage(
  
  titlePanel("Análise Financeira"),
  
  sidebarLayout(
    
    sidebarPanel( 
        h3("Dados de entrada"),
        numericInput("tx_juros", label = "Taxa de juros (%)", value = 10),
        textInput("fluxo_caixa", label = "Fluxo de caixa (separado por vírgulas)",value="-100,-100,100,100,200"),
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
    
    # 
    
  })
  
}


# run  app
shinyApp(ui = ui, server = server)
