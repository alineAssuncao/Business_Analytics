# Análise de Séries Temporais 
# forecast, ARIMA

## Modelando e Prevendo Séries Temporais Lineares
## Uma classe importante de modelos de séries temporais lineares é a família de modelos de média móvel integrada (ARIMA), 
## proposta por Box e Jenkins (1976). Ele assume que o valor atual pode depender apenas dos valores passados da própria 
# série temporal ou de valores passados de algum termo de erro.

# Em geral, ao estudarmos uma série temporal estaremos interessados em dois aspectos:
# 1- Análise e Modelagem da série temporal - descrever a série, verificar suas características mais relevantes e suas possíveis 
# relações com outras séries.
# 2- Previsão da série temporal - a partir de valores passados da série (e talvez de outras séries também) encontrar boas previsões
# de curto prazo de valores futuros da série.

# A dependência entre os valores da série é essencial, pois nos permite gerar previsões de valores futuros da série.
# Diferentes séries possuem diferentes "graus" de previsibilidade. Por exemplo: é mais fácil prever uma série de temperaturas 
# mensais, do que prever a taxa mensal de inflação. Portanto, não se pode garantir que a previsão será sempre eficiente e tudo
# depende das características da série sendo estudada. E quanto mais longe é o futuro das previsões, maior a incerteza.

install.packages("forecast")
library("forecast")
library("zoo")

# Carregando os dados de preços de casas em uma série temporal zoo
# O parâmetro FUN indica que a série temporal é mensal (as.yearmon)
precos <- read.zoo("data/preco_casas.csv", sep = ",", header = TRUE, format = "%Y-%m", FUN = as.yearmon)
head(precos)
View(precos)

# Plot
# Em geral, a primeira coisa que fazemos é gerar um gráfico da série temporal e analisar seu comportamento ao longo do período analisado
plot(precos, main = "Evolução dos Preços de Casas", col = "blue", xlab = "Data", ylab = "Preço")

# Verificando a quantidade de períodos da série de dados
frequency(precos)

# Calculando o retorno simples
hp_ret <- diff(precos) / lag(precos, k = -1) * 100

# Identificação e estimação do modelo
# Modelos ARIMA são modelos sofisticados, que usam as correlações entre as observações em diversos instantes. 
# A ideia por trás dos modelos ARIMA envolve filtros lineares e algum conhecimento de Teoria de Sistemas.
# A identificação da estrutura do modelo é um pouco complicada. Como casos particulares dos modelos ARIMA temos os processos
# AR (Auto Regressivo) e MA (Moving Average).
# Os modelos ARIMA apresentam bons resultados quando a série é relativamente longa.
?auto.arima

# Usamos a função auto.arima fornecida pelo pacote forecast para identificar o modelo ótimo e estimar os coeficientes 
# em uma única etapa. A função possui vários argumentos além da série de retornos (hp_ret). 
# Especificando stationary = TRUE, restringimos a pesquisa a modelos estacionários. 
# De forma similar, seasonal = FALSE restringe a busca a modelos não sazonais. 
# Além disso, selecionamos os critérios de informação de Akaike como a medida da qualidade relativa a ser usada 
# na seleção do modelo.
mod <- auto.arima(hp_ret, stationary = TRUE, seasonal = FALSE, ic = "aic")

# Visualizando o modelo
# O processo de ar2 parece encaixar melhor os dados, de acordo com os Critérios de Informação de Akaike. 
# Mostra autocorrelações parciais não nulas, portanto, um processo de AR de ordem dois parece 
# ser apropriado. Os dois coeficientes AR, o intercepto (que é realmente a média se o modelo contém um termo AR), 
# e os respectivos erros padrão são mostrados. 
mod

# Se o modelo contém coeficientes que são insignificantes, podemos estimar o modelo de novo usando a função arima 
# com o argumento fixo.
confint(mod)

# Uma maneira rápida de validar o modelo é traçar diagnósticos de séries temporais usando o seguinte comando:
tsdiag(mod)

# Nosso modelo parece bom, pois os resíduos padronizados não mostram clusters de volatilidade, 
# não há autocorrelações significativas entre os resíduos de acordo com o gráfico ACF, e o teste 
# Ljung-Box para autocorrelação mostra altos valores de p, assim a hipótese nula de 
# independência Resíduos não pode ser rejeitada. 

# Para avaliar o quão bem o modelo representa os dados da amostra, podemos traçar os retornos mensais brutos 
# (a linha sólida preta fina) versus os valores ajustados (a linha pontilhada vermelha tracejada).
plot(mod$x, lty = 1, main = "Preço de Casas: Dados Brutos vs. Valores com Fit", ylab = "Retorno %", xlab = "Data")
lines(fitted(mod), lty = 2,lwd = 2, col = "red")

# Calculando outras medidas de acurácia
accuracy(mod)

# Prevendo o retorno mensal 3 meses a frente
# Para traçar a previsão com erros padrão, podemos usar o seguinte comando:
predict(mod, n.ahead = 3)

# Assim, esperamos um ligeiro aumento nos preços médios das casas nos próximos três meses, 
# mas com um erro padrão elevado de cerca de 1%. 

# Plot
plot(forecast(mod))

