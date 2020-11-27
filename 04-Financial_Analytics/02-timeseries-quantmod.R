# Modelagem Financeira com Quantmod (Quantitative Financial Modelling Framework)
# https://cran.r-project.org/web/packages/quantmod/quantmod.pdf
# http://www.quantmod.com/

getwd()

# Pacote
install.packages("curl")
library(curl)
install.packages("TTR")
library(TTR)
install.packages("quantmod")
library(quantmod) 

# Download do preço diário das ações da Apple no Yahoo Finance
?getSymbols
getSymbols("AAPL",auto.assign = T)  

# Visualizando o dataset
View(AAPL)
dim(AAPL)  
head(AAPL)  
tail(AAPL) 
class(AAPL)
as.numeric(AAPL$AAPL.Close) 

# Obtendo todas as moedas
moedas <- oanda.currencies
moedas

# Plot
?chartSeries

# Plot do preço diário e volume
chartSeries(AAPL, theme = "white")  
chartSeries(AAPL) 
getSymbols("AAPL", from = "2017-01-02", to = "2017-01-31",auto.assign = T)
head(AAPL)
chartSeries(AAPL,theme = "white")  

# Obtendo dados de cotações das ações da Petrobras da Bovespa
ptbr = c('PETR4.SA','^BVSP')
cot_ptbr = getSymbols(ptbr, from = '2016-01-01', to = '2016-12-31')
cot_ptbr
chartSeries(BVSP)

# Obtendo dados da Bovespa nas últimas 48 horas ou últimos 6 meses
getSymbols("BVMF3.SA", src = "yahoo")
chartSeries(BVMF3.SA, subset = 'last 48 months')
addBBands()
chartSeries(BVMF3.SA, subset = 'last 6 months')
addBBands()

# Outra forma de conectar na Bovespa
library(tseries)
get.hist.quote('^BVSP', '2016-01-01', '2016-12-31') # Indice Bovespa
get.hist.quote('PETR4.SA', '2016-01-01', '2016-12-31') # Petrobras

# Download dos dados de desemprego nos EUA
# FRED - Federal Reserve Economic Data
getSymbols("UNRATE", src = "FRED",auto.assign = T) 
head(UNRATE)
tail(UNRATE)

# Plot da taxa mensal de desemprego
chartSeries(UNRATE, theme = "white")  

# Ajustando o resultado com cálculo de log
getSymbols("AAPL", from = "2017-01-02", to = "2017-01-31")
AAPL.return = diff(log(AAPL$AAPL.Adjusted)) 
chartSeries(AAPL.return, theme = "white")

# Obtendo as taxas de juros do FRED - Federal Reserve Economic Data
# https://fred.stlouisfed.org/
getSymbols("DEXUSEU", src="FRED") 
head(DEXUSEU)
tail(DEXUSEU)

# Ajustando o resultado com cálculo de log
USEU.return = diff(log(DEXUSEU$DEXUSEU))
chartSeries(DEXUSEU, theme = "white")
chartSeries(USEU.return, theme = "white")


### ERRO DE PERMISSÃO DE ACESSO PARA CAPTURAR AS COTAÇÕES
# Obtendo a cotação do dólar nos últimos 2 anos
datainicial = "2017-11-01"
datafinal = "2018-04-01"
cotacoes = getFX("USD/BRL", from = datainicial, to = datafinal, env = NULL)
lineChart(cotacoes)

# Calculando estatísticas
install.packages("fBasics")
library(fBasics) 

# Carregando dados de retorno das ações da 3M
dados = read.table("data/3m-returns.csv", header = T) 
head(dados) 
df_3m = dados[,2]  

# Estatísticas
?basicStats
basicStats(df_3m) 
mean(df_3m)
var(df_3m)
stdev(df_3m) 

# Teste t com média de retorno = 0
?t.test
t.test(df_3m)  

# Teste de skewness
s3 = skewness(df_3m)
T = length(df_3m) 
t3 = s3/sqrt(6/T) 

# Calculando o p-value
pp = 2*(1-pnorm(t3)) 

# Teste Kurtosis
s4 = kurtosis(df_3m)
t4 = s4/sqrt(24/T) 

# Histograma e Gráfico de Densidade
dados = read.table("data/3m-returns.csv", header = T) 
head(dados) 
df_3m = dados[,2]  

# Histograma
hist(df_3m, nclass=30)

# Estimativa de densidade
d1 = density(df_3m) 
range(df_3m)  

# Criando uma sequência com incremento de 0.001 e gerando o plot
x = seq(-.1,.1,.001)
y1 = dnorm(x,mean(df_3m), stdev(df_3m))
plot(d1$x,d1$y,xlab = 'Retorno', ylab = 'Densidade', type = 'l')
lines(x, y1, lty = 2)


# Regressão Linear
# Retorno das ações da IBM x Índice S&P

# Coletando os dados
# Dados de Retorno da Petrobras
# Dados do índice S&P - Standard & Poor's index baseado nas 500 maiores empresas americanas
library(quantmod)
dados_ibm = read.table("data/ibm-returns.csv", header = T)
head(dados_ibm)

# Transformação de log
ibm = log(dados_ibm$ibm+1) 
sp = log(dados_ibm$sp+1)

# Criando time index
tdx = c(1:nrow(dados_ibm))/12+1926 

# Plot
par(mfcol = c(2,1))
plot(tdx, ibm, xlab = 'Ano', ylab = 'Log Return', type = 'l')
title(main = '(a) IBM returns')
plot(tdx, sp, xlab = 'Ano', ylab = 'Log Return', type = 'l') 
title(main = '(b) SP index')

# Obtendo correlação IBM x S&P
cor(ibm, sp)  

# Modelo de regressão linear
modelo = lm(ibm ~ sp)  
summary(modelo)

# Scatter Plot
par(mfcol = c(1,1))
plot(sp, ibm, cex = 0.8)  
abline(0.008,.807) 

# Simulação
# Combinando a transformação de log dos índices de retorno da IBM e S&P
dados_combinados = cbind(ibm,sp) 

# Calculando as médias
medias = apply(dados_combinados, 2, mean) 
medias

# Obtendo a matriz de covariância
?cov
covariancias = cov(dados_combinados) 
covariancias

# Gerando massa de dados aleatórios
library(mnormt) 

# Simulação
x = rmnorm(1029, mean = medias, varcov = covariancias) 
dim(x)

# Plot
plot(x[,2], x[,1], xlab = 'Simulação S&P', ylab = 'Simulção IBM', cex = 0.8)

