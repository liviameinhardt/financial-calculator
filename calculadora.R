## Este modulo possui as funcoes da calculadora

#calcula o VPL dado o fluxo de caixa e a taxa de juros
#considera o primeiro valor do fluxo o investimento inicial
vpl <- function(fluxo_caixa, taxa_juros) {
  n <- length(fluxo_caixa)
  vpl <- sum(fluxo_caixa / (1 + taxa_juros)^(0:(n-1)))
  return(vpl)
}

#calcula a taxa interna de retorno dado um fluxo de caixa em %
# utiliza a funcao uniroot que calcula a raiz do VPL como funcao da taxa de juros no intervalo de 0 a 1
tir <- function(fluxo_caixa) {
  n <- length(fluxo_caixa)
  f <- function(r) {sum(fluxo_caixa / (1 + r)^(0:(n-1)))}
  
  tryCatch({
    tir_ = (uniroot(f, c(0, 1))$root) 
    return(tir_*100 )},
    error = function(e){return('-')} #se a raiz da funcao não existir
  )
  
}


#calcula o retorno do investimeto dado o fluxo de caixa e a taxa de juros
roi <- function(fluxo_caixa, juros) {
  
  #pega a lista de investimeto inicial (os primeiros valores negativos do fluxo)
  investimento_inicial <- c()
  for (i in fluxo_caixa) {
    if (i < 0) investimento_inicial <- c(investimento_inicial, -i)
    else break
  }
  
  roi <- vpl(fluxo_caixa,juros) / vpl(investimento_inicial,juros) #calcula o roi
  return(roi*100)
}


#calcula o tempo de payback simples e descontado dado o fluxo de caixa e a taxa de juros
payback <- function(fluxo_caixa, taxa_juros) {
  
  payback_descontado = '-'
  payback_simples = '-'
  
  #payback simples
  soma_acumulada = cumsum(fluxo_caixa)
  for (tempo in 1:length(soma_acumulada)) { 
    if (soma_acumulada[tempo] > 0) {  
      payback_simples <- (tempo-1)
      break
    } 
  }
  
  #paypack descontado
  n <- length(fluxo_caixa)
  fatores <- (1 + taxa_juros)^(0:(n-1))
  fluxo_presente <- fluxo_caixa/fatores
  soma_acumulada = cumsum(fluxo_presente)
  
  for (tempo in 1:length(soma_acumulada)) { 
    if (soma_acumulada[tempo] > 0) {  
      payback_descontado <- (tempo-1)
      break
    } 
  }
  
  return(list(payback_simples=payback_simples,payback_descontado=payback_descontado))
}
