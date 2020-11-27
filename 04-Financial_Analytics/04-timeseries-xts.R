# Análise de Séries Temporais 
# xts - eXtensible Time Series

# Fornece um tratamento uniforme das diferentes classes de dados baseadas no zoo, 
# maximizando a preservação da informação em formato nativo e permitindo a personalização do nível de usuário,
# ao mesmo tempo em que simplifica a interoperabilidade entre classes

## Pacote xts
## A série de dados retornada pelo pacote quantmod vem na forma de um objeto xts
## Vamos estudar alguns recursos úteis em xts, por meio de dados recuperados do quantmod. 
## Também precisamos carregar mais um pacote: moments 
## O pacote moments calcula skewness (assimetria) e kurtosis (curtose) dos dados. 
install.packages("moments")
install.packages("quantmod")
library(quantmod)
library(xts)
library(moments)  


# Vamos fazer o download dos valores do índice S&P dos últimos 10 anos
# https://www.google.com/finance?cid=700145
### TROCANDO google PELO yahoo. GOOGLE ESTÁ COM ERRO 403 (FORBIDDEN)
getSymbols("SPY", src = "yahoo", from = "2007-01-01",auto.assign = T)
is.xts(SPY) 
head(SPY)
tail(SPY)

# O objeto xts se parece com um dataframe, em que o índice é uma data
# Mas vamos capturar apenas os valores de fechamento
SPY.Close <- SPY[, "SPY.Close"]
is.xts(SPY.Close)

# Se o xts se comporta como um dataframe, por que devo usá-lo em séries temporais?
# Por um simples motivo, ele fornece os dados já com índice de data, o que permite diversas operações,
# sem a necessidade de conversões adicionais
x1 <- SPY['2015-01/2016-12']
head(x1)
x2 <- SPY['/2016-09']
head(x2)
tail(x2)
x3 <- SPY['2016']
tail(x3)
x4 <- SPY['2016-12']
tail(x4)
x5 <- to.period(SPY['2015'], 'months')
tail(x5)
x6 <- to.period(SPY['2015'], 'quarters')
tail(x6)
x7 <- to.monthly(SPY['2016'])
x8 <- to.quarterly(SPY['2016'])

# Extraindo os dados apenas do fechamento
head(SPY.Close)
SPY.Close['2015-01/2016-12']
SPY.Close['/2015-07']
SPY.Close['2016']
SPY.Close['2016-12']

# Plot
plot(SPY.Close, main = "Preços de Fechamento Diário para o Índice SP 500 (SPY)",
     col = "red",xlab = "Data", ylab = "Preço", major.ticks = 'years',
     minor.ticks = FALSE)

# Log returns
SPY.ret <- diff(log(SPY.Close), lag = 1)
SPY.ret <- SPY.ret[-1] 

# Plot
plot(SPY.ret, main = "Preços de Fechamento Diário para o Índice SP 500 (SPY)",
     col = "red", xlab = "Data", ylab = "Retorno", major.ticks = 'years',
     minor.ticks = FALSE)

# Estatísticas
statNames <- c("media", "desvio padrao", "assimetria", "curtose")
SPY.stats <- c(mean(SPY.ret), sd(SPY.ret), skewness(SPY.ret), kurtosis(SPY.ret))
names(SPY.stats) <- statNames
SPY.stats

