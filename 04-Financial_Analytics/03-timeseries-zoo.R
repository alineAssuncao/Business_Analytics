# Análise de Séries Temporais 
# zoo - Z's Ordered Observations
# https://cran.r-project.org/web/packages/zoo/zoo.pdf
# Esse pacote possui métodos para indexação totalmente ordenada
# Destina-se particularmente a séries temporais irregulares de vetores numéricos / matrizes e fatores. 

## Pacote zoo
install.packages("zoo")
library("zoo")

# Carregando dataset de cotações da Apple
?read.zoo
aapl <- read.zoo("data/apple.csv", sep = ",", header = TRUE, format = "%Y-%m-%d")

# Visualizando o Dataset
head(aapl)
tail(aapl)
View(aapl)

# Plot
plot(aapl, main = "Preço de Fechamento das Ações da Apple na NASDAQ", ylab = "Preço (USD)", xlab = "Data")

# Visualizando o valor mais alto de cotação
aapl[which.max(aapl)]

# Quando trabalhamos com séries temporais, o retorno normalmente é mais importante o preço
# Vamos calcular o retorno simples 
ret_simple <- diff(aapl) / lag(aapl, k = -1) * 100

# Podemos também obter estatísticas sobre o retorno simples
# A função coredata do pacote zoo permite extrair apenas os dados (e não as datas) de nosso dataset
?coredata

# Podemos ver que o valor mínimo de perda foi de 51.86%
summary(coredata(ret_simple))

# Verificando o dia em que ocorreu a maior perda
# Uma pesquisa rápida na internet mostra que no dia que houve a perda, a Apple anunciou redução nos lucros,
# com valores abaixo do esperado pelo mercado. Você poderia acrescentar esta informação em seu Financial Analytics,
# junto com indicadores econômicos da sua própria empresa e/ou de outras empresas concorrentes.
ret_simple[which.min(ret_simple)]

# Visualizando o retorno
# Nitidamente tivemos um outlier, o que impacta na média de retorno das ações da Apple
hist(ret_simple, breaks = 100, main = "Histograma de Retornos Simples", xlab = "%")

# Nós podemos restringir nossa análise a subset dos dados, o que em análise de séries temporais, chamamos de window (janela)
# Coletando dados apenas de 2013
aapl_2013 <- window(aapl, start = '2013-01-01', end = '2013-12-31')
aapl_2013[which.max(aapl_2013)]

# Os quartis da distribuição de retorno são interessantes em perspectiva de gestão de risco. 
# Podemos, por exemplo, determinar facilmente o valor de risco de 1 dia usando esta abordagem. 
# Assim, a probabilidade de que o retorno seja inferior a 7% em qualquer dia é de apenas 1%. 
# Mas se este dia ocorrer (e isso ocorrerá aproximadamente 2,5 vezes por ano), 7% é o valor mínimo de perda.
quantile(ret_simple, probs = 0.01)





