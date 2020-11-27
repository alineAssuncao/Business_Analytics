# Marketing Analytics em R
# Módulo 4 - Previsões

# Obs: Caso tenha problemas com a acentuação, consulte este link:
# https://support.rstudio.com/hc/en-us/articles/200532197-Character-Encoding

# Configurando o diretório de trabalho
# Coloque entre aspas o diretório de trabalho que você está usando no seu computador
# Não use diretórios com espaço no nome
setwd("/home/aline/Projetos/3")
getwd()

# Carregando os dados
data = read.delim(file = 'data/compras.txt', header = FALSE, sep = '\t', dec = '.')

# Adicionando cabeçalho e adicionando uma coluna extra com o ano da compra
colnames(data) = c('cliente_id', 'valor_compra', 'data_compra')
data$data_compra = as.Date(data$data_compra, "%Y-%m-%d")
data$days_since  = as.numeric(difftime(time1 = "2016-01-01", time2 = data$data_compra, units = "days"))
data$ano_compra = as.numeric(format(data$data_compra, "%Y"))

# Resumo dos dados após modificação
head(data)
summary(data)

# Computando indicadores chave de Marketing com SQL
library(sqldf)

# Computando variáveis de 1 ano atrás
clientes_2014 = sqldf("SELECT cliente_id,
                               MIN(days_since) - 365 AS 'compra_mais_recente',
                               MAX(days_since) - 365 AS 'primeira_compra',
                               COUNT(*) AS 'frequencia',
                               AVG(valor_compra) AS 'media_compra',
                               MAX(valor_compra) AS 'valor_maximo_compra'
                        FROM data
                        WHERE days_since > 365
                        GROUP BY 1")

# Computando faturamento gerado por clientes em 2015
faturamento_2015 = sqldf("SELECT cliente_id, SUM(valor_compra) AS 'faturamento_2015'
                      FROM data
                      WHERE ano_compra = 2015
                      GROUP BY 1")

# Merge dataset de clientes de 2014 com faturamento de 2015
in_sample = merge(clientes_2014, faturamento_2015, all.x = TRUE)
in_sample$faturamento_2015[is.na(in_sample$faturamento_2015)] = 0
in_sample$active_2015 = as.numeric(in_sample$faturamento_2015 > 0)

# Visualizando
head(in_sample)
summary(in_sample)


#####---------------- Construindo o Modelo Preditivo -----------------------

# Construindio um Modelo de Regressão Linear Múltipla

# Para o valor monetário, selecionamos apenas os clientes que fizeram pelo menos uma compra
z = which(in_sample$active_2015 == 1)
head(in_sample[z, ])
summary(in_sample[z, ])

# Ajustando o modelo com o valor monetário de clientes que fizeram pelo menos uma compra
amount.model = lm(formula = faturamento_2015 ~ media_compra + valor_maximo_compra, data = in_sample[z, ])
summary(amount.model)

# Plot 
plot(x = in_sample[z, ]$faturamento_2015, y = amount.model$fitted.values)

# Calibrando o valor monetário com função de log
amount.model = lm(formula = log(faturamento_2015) ~ log(media_compra) + log(valor_maximo_compra), data = in_sample[z, ])
summary(amount.model)

# Plot 
plot(x = log(in_sample[z, ]$valor_maximo_compra), y = amount.model$fitted.values)


#####---------------- Aplicando o Modelo aos Dados Atuais -----------------

# Dados atuais
clientes_2015 = sqldf("SELECT cliente_id,
                               MIN(days_since) AS 'compra_mais_recente',
                               MAX(days_since) AS 'primeira_compra',
                               COUNT(*) AS 'frequencia',
                               AVG(valor_compra) AS 'media_compra',
                               MAX(valor_compra) AS 'valor_maximo_compra'
                        FROM data GROUP BY 1")

# Prevendo a variável target com base em dados atuais
clientes_2015$revenue_predicted = exp(predict(object = amount.model, newdata = clientes_2015))
summary(clientes_2015$revenue_predicted)

# Quantos clientes possuem um faturamento esperado superior a R$500
z = which(clientes_2015$revenue_predicted > 500)
print(length(z))






