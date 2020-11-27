# Fraud Analytics - Análise Exploratória

# Obs: Caso tenha problemas com a acentuação, consulte este link:
# https://support.rstudio.com/hc/en-us/articles/200532197-Character-Encoding

# Configurando o diretório de trabalho
# Coloque entre aspas o diretório de trabalho que você está usando no seu computador
# Não use diretórios com espaço no nome
setwd("/home/aline/Projetos/6")
getwd()


## Carregando e visualizando os dados
dados = read.csv("data/dataset_source.csv")
View(dados)
head(dados)
tail(dados)
str(dados)


# Convertendo a coluna AMOUNT de fator para numérico (double)
str(dados$AMOUNT)
any(is.na(dados$AMOUNT))
dados$AMOUNT <- as.double(dados$AMOUNT)
dados$AMOUNT <- as.double(as.character(dados$AMOUNT))
str(dados$AMOUNT)
any(is.na(dados$AMOUNT))


# Convertendo a coluna MERCHZIP de numérico para fator
str(dados$MERCHZIP)
any(is.na(dados$MERCHZIP))
dados$MERCHZIP <- as.factor(dados$MERCHZIP)
str(dados$MERCHZIP)
any(is.na(dados$MERCHZIP))
str(dados)


## Explorando variáveis numéricas
min(dados$AMOUNT)
max(dados$AMOUNT)
mean(dados$AMOUNT)
median(dados$AMOUNT)
quantile(dados$AMOUNT)
range(dados$AMOUNT)

# Medidas de Dispersão
# Ao interpretar a variância, números maiores indicam que 
# os dados estão espalhados mais amplamente em torno da 
# média. O desvio padrão indica, em média, a quantidade 
# de cada valor diferente da média.
var(dados$AMOUNT)
sd(dados$AMOUNT)


# Plot

# Boxplot
# Leitura de Baixo para Cima - Q1, Q2 (Mediana) e Q3
boxplot (dados$AMOUNT, main = "Boxplot para Quantidade Gasta no Cartão de Crédito", ylab = "Valor (USD$)")


# Histograma
# Indicam a frequência de valores dentro de cada bin (classe de valores)
hist(dados$AMOUNT, main = "Histograma para Quantidade Gasta no Cartão de Crédito", xlab = "Valor (USD$)")


## Explorando variáveis categóricas

# Criando tabelas de contingência - representam uma única variável categórica
# Lista as categorias das variáveis nominais
str(dados)
table(dados$MERCHZIP)
model_table <- table(dados$MERCHZIP)
prop.table(model_table)


# Arrendondando os valores
model_table <- table(dados$MERCHZIP)
model_table <- prop.table(model_table) * 100
round(model_table, digits = 1)


# PlotS
str(dados$MERCHZIP)
barplot(table(dados$MERCHZIP), col = "black", main = "Número de Transações por Postal Code")
str(dados$DATE)
barplot(table(dados$DATE), col = "black", main = "Número de Transações por Dia")
str(dados$MERCHNUM)
barplot(table(dados$MERCHNUM), col = "black", main = "Número de Transações por Comerciante")
str(dados$MERCHSTATE)
barplot(table(dados$MERCHSTATE), col = "black", main = "Número de Transações por Estado")
str(dados$TRANSTYPE)
barplot(table(dados$TRANSTYPE), col = "black", main = "Número de Transações por Tipo de Transação")


## Visualizando as fraudes por mês
install.packages("lubridate")
library(lubridate)
library(dplyr)
library(ggplot2)


# Extrai todas as linhas da coluna 1 a coluna 10 (no caso de haver mais colunas do que o necessário)
dados = dados[,1:10]
head(dados)

# Extraindo o mê da data e incluindo em uma nova coluna
dados$DATE = mdy(dados$DATE)
dados$month = month(dados$DATE)


# Sumarizando os dados e gravando em um novo dataframe
dados2 = dados %>%
  group_by(month)%>%
  summarise(fraud = sum(Fraudlabel))


# Convertendo a coluna de mês para fator
dados2$month = as.factor(dados2$month)


# Plot
ggplot(dados2,aes(x = month, y = fraud))+
  geom_bar(stat = "identity", fill = "blue")





