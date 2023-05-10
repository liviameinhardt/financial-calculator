#este modulo possui as funcoes extras para processar os inputs do usuario e os outputs do app

# transforma a string do input em um vetor 
string_para_vetor <- function(string) {
  vetor <- strsplit(string, ",")[[1]]
  vetor <- as.numeric(vetor)
  return(vetor)
}

# gera a tabela com os resultados da análise
gerar_tabela <- function(fluxo_caixa, juros) {
  
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
