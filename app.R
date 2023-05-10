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
        actionButton("calcular", "Calcular"),
    ),
      
    
    mainPanel(
        tabsetPanel(
          
          tabPanel("Resultados",
                   h3("Resultados da análise"),
                   tableOutput("tabela_resultados")
          ),
          
          # tabPanel("Gráfico",
          #          h3("Gráfico do fluxo de caixa"),
          #          plotOutput("grafico_fluxo_caixa")
          # )
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
    output$tabela_resultados <- renderTable({ gerar_tabela(fluxo_caixa, tx_juros) })
    
    #output$grafico_fluxo_caixa <- renderPlot({gerar_grafico(fluxo_caixa)})
    
  })
  
}


# run  app
shinyApp(ui = ui, server = server)
