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

#este modulo possui as funcoes extras para processar os inputs do usuario e os outputs do app
library(plotly)

# transforma a string do input em um vetor 
string_para_vetor <- function(string) {
  vetor <- strsplit(string, ",")[[1]]
  vetor <- as.numeric(vetor)
  return(vetor)
}

# gera a tabela com os resultados da analise
gerar_tabela_investimento <- function(fluxo_caixa, juros) {
  
  resultados <- c(
    VPL = round(vpl(fluxo_caixa, juros),2),
    TIR = tir(fluxo_caixa),
    ROI = round(roi(fluxo_caixa, juros),2),
    Payback_Simples = payback(fluxo_caixa, juros)$payback_simples,
    Payback_Descontado = payback(fluxo_caixa, juros)$payback_descontado
    
  )
  tabela <- data.frame(Resultados = names(resultados), Valor = resultados)

  return(tabela)
}

#gera o grafico de barras do fluxo de caixa
grafico_fluxo_caixa <- function(fluxo_caixa){
  
  df <- data.frame(Movimentacao = 0:(length(fluxo_caixa)-1),
                   Valor = fluxo_caixa, 
                   Tipo = ifelse(fluxo_caixa > 0, "Entrada de caixa", "Saida de caixa"))
  
  fig <- ggplot(df, aes(x = Movimentacao, y = Valor, fill = Tipo) ) + 
    geom_bar(stat = "identity", width = 0.5) +
    geom_text(aes(label = Valor), position = position_stack(vjust = 0.5), color = "black") +
    scale_fill_manual(values = c("Entrada de caixa" = "green", "Saida de caixa" = "red")) +
    theme_minimal() +
    ggtitle("Fluxo de Caixa") + xlab("Tempo") + ylab("Valores") +
    guides(fill = "none") # remove a legenda das cores
  
  # Converte o grafico ggplot para um grafico plotly interativo
  p <- ggplotly(fig, tooltip = c("Movimentacao", "Valor", "Tipo"))
  
  return(p)
}


#gera a tabela das metricas em funcao da taxa de juros
gerar_tabela_juros <- function(fluxo_caixa,taxas_juros){
  
  #taxas de juros para considerar
  #taxas_juros = c(0.01,0.05,0.1,0.15,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9)
  
  # Cria um dataframe vazio para armazenar as metricas
  resultados <- data.frame(taxa_juros = taxas_juros,
                           vpl = numeric(length(taxas_juros)),
                           roi = numeric(length(taxas_juros)),
                           payback_simples = numeric(length(taxas_juros)),
                           payback_descontado = numeric(length(taxas_juros)))
  
  
  # Preenche o dataframe com as metricas para cada taxa de juros
  for (i in seq_along(taxas_juros)) {
    resultados$vpl[i] <- vpl(fluxo_caixa, taxas_juros[i])
    resultados$roi[i] <- roi(fluxo_caixa, taxas_juros[i])
    payback_resultados <- payback(fluxo_caixa, taxas_juros[i])
    resultados$payback_simples[i] <- payback_resultados$payback_simples
    resultados$payback_descontado[i] <- payback_resultados$payback_descontado
  }
  
  resultados$taxa_juros = resultados$taxa_juros*100
  
  return(resultados)
}


#gera o grafico das metricas em funcao da taxa de juros, pintando a coluna da taxa selecionada de verde

grafico_juros <- function(df,nome_coluna,taxa_selecionada){
  
  resultados_selecionados <- df[, c("taxa_juros", nome_coluna)]
  resultados_selecionados$taxa <- ifelse(resultados_selecionados$taxa_juros == taxa_selecionada, "Selecionada", "Outras")
  
  fig <- plot_ly(data = resultados_selecionados,
                 x = ~taxa_juros, y = ~.data[[nome_coluna]],
                 color = ~taxa, colors = c(Selecionada = "red", Outras = "grey"),
                 type = "scatter", mode = "markers") %>%
    
    layout(title = paste0(toupper(nome_coluna), " em Funcao da Taxa de Juros"),
           xaxis = list(title = "Juros"), yaxis = list(title = nome_coluna), showlegend = TRUE)
  
  return(fig)
}

