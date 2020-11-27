# Análise Descritiva em R

# Obs: Caso tenha problemas com a acentuação, consulte este link:
# https://support.rstudio.com/hc/en-us/articles/200532197-Character-Encoding

# Configurando o diretório de trabalho
# Coloque entre aspas o diretório de trabalho que você está usando no seu computador
# Não use diretórios com espaço no nome
setwd("/home/aline/Projetos/2")
getwd()

# Coletando e carregando os dados
carros <- read.csv("data/carros.csv", stringsAsFactors = FALSE)

# Sumário da estrutura do dataset
str(carros)
head(carros)

## Explorando variáveis numéricas

# Sumário
summary(carros$year)
summary(carros[c("price", "mileage")])

# Calculando a média
(36000 + 44000 + 56000) / 3
mean(c(36000, 44000, 56000))
mean(carros$price)

# Mediana
median(c(36000, 44000, 56000))
median(carros$price)

# Range - Min e Max
range(carros$price)

# Diferença do Range (Max -  Min)
diff(range(carros$price))

# IQR - Interquantile Range
IQR(carros$price)

# Percentil
quantile(carros$price)

# 99th Percentil
quantile(carros$price, probs = c(0.01, 0.99))

# quintiles
quantile(carros$price, seq(from = 0, to = 1, by = 0.20))

# Boxplot
boxplot(carros$price, main = "Boxplot Preços-Carros", ylab = "Preço (R$)")
boxplot(carros$mileage, main="Boxplot Kilometragem", ylab = "Kilometragem (Km)")

# Histograma
hist(carros$price, main = "Histogram Preços-Carros", xlab = "Preço (R$)")
hist(carros$mileage, main = "Histogram Kilometragem", xlab = "Kilometragem (Km)")

# Dotchart (Gráfico univariado)
dotchart(carros$price)

# Variância e Desvio Padrão
var(carros$price)
sd(carros$price)
var(carros$mileage)
sd(carros$mileage)

## Explorando variáveis numéricas

# Tabela de Frequência
table(carros$year)
table(carros$model)
table(carros$color)

# Proporções da Tabela de Frequência
model_table <- table(carros$model)
prop.table(model_table)

# Ajuste do resultado das proporções
color_table <- table(carros$model)
color_pct <- prop.table(color_table) * 100
round(color_pct, digits = 1)

# Explorando relacionamento entre as variáveis

# Scatter Plot (gráfico de disperção - bivariado)
plot(x = carros$mileage, y = carros$price,
     main = "Scatterplot Preço x Kilometragem",
     xlab = "Kilometragem (Km)",
     ylab = "Preço (R$)")


# Pacote psych
install.packages("psych")
library(psych)
describe(carros)

# Checando valores missing
is.na(carros$price)

# Aggregate
?aggregate
aggregate(carros$price ~ carros$year, FUN = mean, data = carros)
aggregate(carros$price ~ carros$year, FUN = median, data = carros)












