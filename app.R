library(ggplot2)
library(shiny)
library(shinythemes)
source("utils.R")
source("calculadora.R")
library(bslib)
library(shinyWidgets)
thematic::thematic_shiny()


# define a interface UI
ui <- navbarPage(
  title = "Análise Financeira",
  prettySwitch(
    inputId = "themeToggle",
    label = "DarkMode"
  ),
  tags$head(
    tags$style(HTML("
      .form-group {
        margin-bottom: 15px;
      }
      .shiny-input-container {
        width: 100%;
      }
      
      .selectize-control {
        height: 38px;
        padding: 0;
        border-radius: 4px;
        border: 1px solid #ccc;
        background-color: #f9f9f9;
        color: #333;
      }
      .selectize-input {
        height: 38px;
        padding: 6px 12px;
        background-color: #f9f9f9;
        color: #333;
      }
      .selectize-dropdown {
        background-color: #f9f9f9;
        color: #333;
        border: 1px solid #ccc;
      }
      input[type=number] {
        height: 38px;
        padding: 6px 12px;
        background-color: #f9f9f9;
        color: #333;
        border-radius: 4px;
        border: 1px solid #ccc;
      }
    "))
  ),
  includeScript(path = "themes.js"),
  tabPanel("Viabilidade do Projetos",
           fluidPage(
             sidebarLayout(
               sidebarPanel(
                 h3("Dados de entrada"),
                 numericInputIcon("tx_juros", label = "Taxa de juros", value = 10,icon = list(NULL, icon("percent"))),
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
                 numericInputIcon("capital", "Capital Inicial", value = 1000, min = 0, icon = list(icon("dollar-sign"))),
                 fluidRow(
                   column(7,
                          numericInputIcon("taxa", "Taxa de Juros", value = 5, min = 0,icon = list(NULL, icon("percent"))),
                          numericInput("tempo", "Tempo", value = 1, min = 0),
                   ),
                   column(5,
                          selectInput("taxa_tipo", "Intervalo", choices = c("Anual" = "ano", "Mensal" = "mes")),
                          selectInput("tempo_tipo", "Unidade", choices = c("Anos" = "anos", "Meses" = "meses")),
                   ),
               ),
               actionButton("calcular3", "Calcular"),
               ),
               mainPanel(
                 fluidRow(
                   column(4, 
                          wellPanel(
                            h5("Valor total final"),
                            textOutput("totalFinal")
                          )
                   ),
                   column(4, 
                          wellPanel(
                            h5("Valor total investido"),
                            textOutput("totalInvestido")
                          )
                   ),
                   column(4, 
                          wellPanel(
                            h5("Total em juros"),
                            textOutput("totalJuros"),
                          )
                   )
                  ),
               )
             )
          )
  ),
  
  tabPanel("Juros Composto",
           
           fluidPage(
             # Add content here
             sidebarLayout(
               sidebarPanel(
                 h3("Dados de entrada"),
                 numericInputIcon("capital2", "Capital Inicial", value = 1000, min = 0, icon = list(icon("dollar-sign"))),
                 fluidRow(
                   column(7,
                          numericInputIcon("taxa2", "Taxa de Juros", value = 5, min = 0,icon = list(NULL, icon("percent"))),
                          numericInput("tempo2", "Tempo", value = 1, min = 0),
                   ),
                   column(5,
                          selectInput("taxa_tipo2", "Intervalo", choices = c("Anual" = "ano", "Mensal" = "mes")),
                          selectInput("tempo_tipo2", "Unidade", choices = c("Anos" = "anos", "Meses" = "meses")),
                   ),
                 ),
                 actionButton("calcular4", "Calcular"),
               ),
               mainPanel(
                 fluidRow(
                   column(4, 
                          wellPanel(
                            h5("Valor total final"),
                            textOutput("totalFinal2")
                          )
                   ),
                   column(4, 
                          wellPanel(
                            h5("Valor total investido"),
                            textOutput("totalInvestido2")
                          )
                   ),
                   column(4, 
                          wellPanel(
                            h5("Total em juros"),
                            textOutput("totalJuros2"),
                          )
                   )
                 ),
               )
             )
           )
  ),
  tabPanel("Estatística",
           
          fluidPage(
           sidebarLayout(
             sidebarPanel(
               h3("Dados de entrada"),
               numericInput("populacao", label = "População", value = 210000000, min=0),
               numericInputIcon("grau_confianca", label = "Grau de Confiança", value = 95, min=0, max=100,icon = list(NULL, icon("percent"))),
               numericInputIcon("margem_erro", label = "Margem de Erro", value = 1, min=0, max=100,icon = list(NULL, icon("percent"))),
               numericInputIcon("proporcao", label = "Proporção ",icon = list(NULL, icon("percent")), value = 50, min=0, max=100),
               actionButton("calcular2", "Calcular")
             ),
             mainPanel(
               #tabsetPanel(
                 #tabPanel("Resultados",
                    fluidRow(
                      column(4, 
                             wellPanel(
                               h5("Tamanho da Amostra"),
                               textOutput("tamanho_amostra")
                             )
                      ),
                      column(5, 
                             wellPanel(
                               h5("Erro Amostral - Proporção (%)"),
                               textOutput("erro_amostral_prop")
                             )
                      )
                    )
                 #)
               #)
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
    
    output$erro_amostral_prop <- renderText({
      erro_amostral_prop <- calcular_erro_amostral_prop(confianca, proporcao, populacao)
      paste(erro_amostral_prop)
    })
    
  })
  
  # Botão de calcular juros simples
  observeEvent(input$calcular3, {
    capital <- input$capital
    tempo <- input$tempo
    taxa <- input$taxa / 100
    # Se a taxa é mensal e o tempo é anual, multiplica a taxa por 12
    if (input$taxa_tipo == "mes" && input$tempo_tipo == "anos") {
      taxa <- taxa * 12
    }
    
    # Se a taxa é anual e o tempo é mensal, divide o tempo por 12
    else if (input$taxa_tipo == "ano" && input$tempo_tipo == "meses") {
      taxa <- taxa / 12
    }
    
    # calcula o montante
    juros <- capital * taxa * tempo
    montante <- capital + juros
    
    output$totalInvestido <- renderText({
      paste0("R$ ", capital)
    })
    output$totalFinal <- renderText({
      paste0("R$ ", round(montante, 2))
    })
    output$totalJuros <- renderText({
      paste0("R$ ", round(juros, 2))
    })
  })
  
  # Botão de calcular juros composto
  observeEvent(input$calcular4, {
    capital <- input$capital2
    tempo <- input$tempo2
    taxa <- input$taxa2 / 100
    # Se a taxa é mensal e o tempo é anual, multiplica a taxa por 12
    if (input$taxa_tipo2 == "mes" && input$tempo_tipo2 == "anos") {
      tempo <- tempo * 12
    }
    
    # Se a taxa é anual e o tempo é mensal, divide o tempo por 12
    else if (input$taxa_tipo2 == "ano" && input$tempo_tipo2 == "meses") {
      tempo <- tempo / 12
    }
    
    # calcula o montante
    montante <- capital*(taxa+1)**tempo
    juros <- montante - capital
    
    output$totalInvestido2 <- renderText({
      paste0("R$ ", capital)
    })
    output$totalFinal2 <- renderText({
      paste0("R$ ", round(montante, 2))
    })
    output$totalJuros2 <- renderText({
      paste0("R$ ", round(juros, 2))
    })
  })
  
}


# run  app
shinyApp(ui = ui, server = server)
