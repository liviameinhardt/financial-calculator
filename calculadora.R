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
    error = function(e){return('-')} #se a raiz da funcao nao existir
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
    if (soma_acumulada[tempo] >= 0) {  
      payback_simples = (tempo-1) + (soma_acumulada[tempo-1] + fluxo_caixa[tempo])/fluxo_caixa[tempo]
      break
    } 
  }
  
  #paypack descontado
  n <- length(fluxo_caixa)
  fatores <- (1 + taxa_juros)^(0:(n-1))
  fluxo_presente <- fluxo_caixa/fatores
  soma_acumulada = cumsum(fluxo_presente)
  
  for (tempo in 1:length(soma_acumulada)) { 
    if (soma_acumulada[tempo] >= 0) {  
      payback_descontado = (tempo-1) + (soma_acumulada[tempo-1] + fluxo_presente[tempo])/fluxo_presente[tempo]
      break
    } 
  }
  
  return(list(payback_simples=payback_simples,payback_descontado=payback_descontado))
}


calcular_tamanho_amostra <- function(populacao, grau_confianca, margem_erro, proporcao = 0.5) {
  # Calcula o valor critico (Z-score) baseado no grau de confianca
  z <- qnorm((1 + grau_confianca) / 2)
  n <- (z^2 * proporcao * (1 - proporcao)) / (margem_erro^2)
  
  if (!is.infinite(n)) {
    n <- ceiling(n)
  }
  
  if (!is.null(populacao) && n > populacao) {
    n <- populacao
  }
  
  return(n)
}

calcular_erro_amostral_prop <- function(grau_confianca, proporcao, tamanho_populacao) {
  z <- qnorm((1 + grau_confianca) / 2)  # Valor critico baseado no nivel de confianca
  
  erro_amostral <- z * sqrt((proporcao * (1 - proporcao)) / tamanho_populacao)
  
  return(round(erro_amostral*100, digits=3))
}

