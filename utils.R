#este modulo possui as funcoes extras para processar os inputs do usuario e os outputs do app

# transforma a string do input em um vetor 
string_para_vetor <- function(string) {
  vetor <- strsplit(string, ",")[[1]]
  vetor <- as.numeric(vetor)
  return(vetor)
}

# gera a tabela com os resultados da análise
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
  
      df <- data.frame(valores = fluxo_caixa, cor = ifelse(fluxo_caixa > 0, "verde", "vermelho"))
  
      fig = ggplot(df, aes(x = 0:(length(valores)-1), y = valores, fill = cor)) + 
      geom_bar(stat = "identity", width = 0.5) +
      geom_text(aes(label = valores), position = position_stack(vjust = 0.5), color = "black") +
      scale_fill_manual(values = c(verde = "green", vermelho = "red")) +
      theme_minimal() +
      ggtitle("Fluxo de Caixa") + xlab("Tempo") + ylab("Valores")+
      guides(fill = "none") # remove a legenda das cores
  
      return(fig)
      
}

#gera a tabela das metricas em funcao da taxa de juros
gerar_tabela_juros <- function(fluxo_caixa){
  
  #taxas de juros para considerar
  taxas_juros = c(0.01,0.05,0.1,0.15,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9)
  
  # Cria um dataframe vazio para armazenar as métricas
  resultados <- data.frame(taxa_juros = taxas_juros,
                           vpl = numeric(length(taxas_juros)),
                           roi = numeric(length(taxas_juros)),
                           payback_simples = numeric(length(taxas_juros)),
                           payback_descontado = numeric(length(taxas_juros)))
  
  
  # Preenche o dataframe com as métricas para cada taxa de juros
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
  resultados_selecionados$taxa = ifelse(resultados_selecionados$taxa_juros ==taxa_selecionada, "Selecionada", "Outras")
  
  fig = ggplot(resultados_selecionados, aes(x = taxa_juros, y = df[[nome_coluna]], color = taxa)) + 
    geom_point( ) +
    scale_fill_manual(values = c(Selecionada = "green", Outras = "grey"))+
    theme_minimal() +
    ggtitle(paste0( toupper(nome_coluna)," em Funçao da Taxa de Juros")) + xlab("Juros") + ylab(nome_coluna)+
    guides(title='Taxa') # remove a legenda das cores
  
  return(fig)
  
}


