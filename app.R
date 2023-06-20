# MIT License
#
# Copyright (c) 2023 Ademir, Ciro, Edilton, Iara, Joao Victor, Livia, Lucas
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in all
# copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.

library(ggplot2)
library(plotly)
library(shiny)
library(shinythemes)
source("utils.R")
source("calculadora.R")
library(bslib)
library(shinyWidgets)
thematic::thematic_shiny()


# define a interface UI
ui <- navbarPage(
  footer = tags$footer(style = "position: fixed; bottom: 0; width: 100%; text-align: center; background-color: #f8f9fa;",
                       tags$p("MIT License"),
                       tags$p("Copyright (c) - 2023 Ademir, Ciro, Edilton, Iara, Joao Victor, Livia, Lucas")
  ),
  title = "Análise Financeira",
  position = "fixed-top",
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
      body > .container-fluid {
        padding-top: 70px;  # Adjust this value depending on the height of your navbar
      }
      
      .selectize-control {
        height: 38px;
        padding: 0;
        border-radius: 4px;
        border: 0px solid #ccc;
        background-color: #f4f9f8;
        color: #333;
      }
      .selectize-input {
        height: 38px;
        padding: 6px 12px;
        background-color: #f9f9f9;
        color: #333;
      }
      .selectize-dropdown {
        background-color: #DCDCDC;
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
      #mySidebar {
        position: fixed;
        width: 300px;
      }
      
      #myMain {
        margin-left: 310px;
      }
      
  
      
      
    "))
  ),
  includeScript(path = "themes.js"),
  tabPanel("Viabilidade de Projetos",
           fluidPage(
             
             sidebarLayout(
               sidebarPanel(
                 id = "mySidebar",
                 h3("Dados de entrada"),
                 numericInputIcon("tx_juros", label = "Taxa de juros", value = 10,icon = list(NULL, icon("percent"))),
                 textInput("fluxo_caixa", label = "Fluxo de caixa (separado por vírgulas)", value = "-100,-100,100,100,200"),
                 textInput("tx_juros_tabela", label = "Taxas para Comparação  (separado por vírgulas)", value = "0.1,0.5,1,2,3,5,7,9,11,13,15,20"),
                 actionButton("calcular", "Calcular")
               ),
               
               mainPanel(
                 
                 tabsetPanel(
                   tabPanel("Investimentos",
                            h2("Resultados da Análise"),
                            tableOutput("tabela_resultados"),
                            plotlyOutput("grafico_fluxo_caixa")
                   ),
                   
                   tabPanel("Taxa de Juros",
                            h2("Métricas em Funçao da Taxa de Juros"),
                            tableOutput("tabela_resultados_juros"),
                            plotlyOutput("grafico_vpl"),
                            plotlyOutput("grafico_roi"),
                            plotlyOutput("grafico_payback_simples"),
                            plotlyOutput("grafico_payback_descontado")
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
                 ),                 fluidRow(
                   column(12,
                          plotlyOutput("graficoJurosSimples")
                   ))
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
                 ),                 fluidRow(
                   column(12,
                          plotlyOutput("graficoJurosCompostos")
                   ))
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
                   ),               fluidRow(
                     column(12,
                            plotlyOutput("graficoProporcao")
                     ))
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
    tx_juros_tabela <- string_para_vetor(input$tx_juros_tabela)/100
    tx_juros <- input$tx_juros/100
    
    if (!(tx_juros %in% tx_juros_tabela)) {
      tx_juros_tabela <- append(tx_juros_tabela, tx_juros)
    }
    
    #trata o output
    
    #analise de investimento
    output$tabela_resultados <- renderTable({ gerar_tabela_investimento(fluxo_caixa, tx_juros) }, class = "custom-table")
    output$grafico_fluxo_caixa <- renderPlotly({grafico_fluxo_caixa(fluxo_caixa)})
    
    
    #analise de taxa de juros
    tabela_func_juros <-  gerar_tabela_juros(fluxo_caixa,tx_juros_tabela)
    output$tabela_resultados_juros <- renderTable({
      df_temp <- tabela_func_juros
      colnames(df_temp) <- c("Taxa de Juros", "VPL", "ROI", "Payback Simples", "Payback Descontado")
      df_temp
    })
    output$grafico_vpl <- renderPlotly({grafico_juros(tabela_func_juros,"vpl",tx_juros*100)})
    output$grafico_roi <- renderPlotly({grafico_juros(tabela_func_juros,"roi",tx_juros*100)})
    output$grafico_payback_simples <- renderPlotly({grafico_juros(tabela_func_juros,"payback_simples",tx_juros*100)})
    output$grafico_payback_descontado <- renderPlotly({grafico_juros(tabela_func_juros,"payback_descontado",tx_juros*100)})
    
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
    
    output$graficoProporcao <- renderPlotly({
      proporcao_estimada <- proporcao
      erro_amostral <- calcular_erro_amostral_prop(confianca, proporcao, populacao)
      
      x <- seq(proporcao_estimada - 3 * erro_amostral, proporcao_estimada + 3 * erro_amostral, length.out = 100)

      densidade <- dnorm(x, mean = proporcao_estimada, sd = erro_amostral)
      
      df <- data.frame(x = x, densidade = densidade)
      ggplot(df, aes(x = x, y = densidade)) +
        geom_line() +
        geom_area(fill = "blue", alpha = 0.3) +
        labs(title = "Distribuicao de Proporcao Estimada",
             x = "Proporcao",
             y = "Densidade")
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
    
    tempo_seq <- seq(0, tempo)
    montante_seq <- capital + capital * taxa * tempo_seq
    data <- data.frame(tempo = tempo_seq, montante = montante_seq)
    
    output$graficoJurosSimples <- renderPlotly({
      ggplot(data, aes(x = tempo, y = montante)) +
        geom_line() +
        labs(x = "Tempo", y = "Montante", title = "Evolucao do Montante ao Longo do Tempo")
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
    
    tempo_seq <- seq(0, tempo)
    montante_seq <- capital * (1 + taxa)^tempo_seq
    data <- data.frame(tempo = tempo_seq, montante = montante_seq)
    
    output$graficoJurosCompostos <- renderPlotly({
      ggplot(data, aes(x = tempo, y = montante)) +
        geom_line() +
        labs(x = "Tempo", y = "Montante", title = "Evolucao do Montante ao Longo do Tempo")
    })
  })
  
}


# run  app
shinyApp(ui = ui, server = server)
