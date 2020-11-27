# Marketing Analytics em R
# Módulo 5 - Life Time Value

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

# Segmentos de Clientes de 2015
clientes_2015 = sqldf("SELECT cliente_id,
                               MIN(days_since) AS 'compra_mais_recente',
                               MAX(days_since) AS 'primeira_compra',
                               COUNT(*) AS 'frequencia',
                               AVG(valor_compra) AS 'media_compra'
                        FROM data GROUP BY 1")

clientes_2015$segmento = "NA"
clientes_2015$segmento[which(clientes_2015$compra_mais_recente > 365*3)] = "Inativo"
clientes_2015$segmento[which(clientes_2015$compra_mais_recente <= 365*3 & clientes_2015$compra_mais_recente > 365*2)] = "Frio"
clientes_2015$segmento[which(clientes_2015$compra_mais_recente <= 365*2 & clientes_2015$compra_mais_recente > 365*1)] = "Quente"
clientes_2015$segmento[which(clientes_2015$compra_mais_recente <= 365)] = "Ativo"
clientes_2015$segmento[which(clientes_2015$segmento == "Quente" & clientes_2015$primeira_compra <= 365*2)] = "Novo Quente"
clientes_2015$segmento[which(clientes_2015$segmento == "Quente" & clientes_2015$media_compra < 100)] = "Quente - Valor Baixo"
clientes_2015$segmento[which(clientes_2015$segmento == "Quente" & clientes_2015$media_compra >= 100)] = "Quente - Valor Alto"
clientes_2015$segmento[which(clientes_2015$segmento == "Ativo" & clientes_2015$primeira_compra <= 365)] = "Novo Ativo"
clientes_2015$segmento[which(clientes_2015$segmento == "Ativo" & clientes_2015$media_compra < 100)] = "Ativo - Valor Baixo"
clientes_2015$segmento[which(clientes_2015$segmento == "Ativo" & clientes_2015$media_compra >= 100)] = "Ativo - Valor Alto"

clientes_2015$segmento = factor(x = clientes_2015$segmento, levels = c("Inativo", "Frio",
                                                                       "Quente - Valor Alto", "Quente - Valor Baixo", "Novo Quente",
                                                                       "Ativo - Valor Alto", "Ativo - Valor Baixo", "Novo Ativo"))

# Segmentos de Clientes de 2014
clientes_2014 = sqldf("SELECT cliente_id,
                               MIN(days_since) - 365 AS 'compra_mais_recente',
                               MAX(days_since) - 365 AS 'primeira_compra',
                               COUNT(*) AS 'frequencia',
                               AVG(valor_compra) AS 'media_compra'
                        FROM data
                        WHERE days_since > 365
                        GROUP BY 1")

clientes_2014$segmento = "NA"
clientes_2014$segmento[which(clientes_2014$compra_mais_recente > 365*3)] = "Inativo"
clientes_2014$segmento[which(clientes_2014$compra_mais_recente <= 365*3 & clientes_2014$compra_mais_recente > 365*2)] = "Frio"
clientes_2014$segmento[which(clientes_2014$compra_mais_recente <= 365*2 & cl processarArquivoS2306ientes_2014$compra_mais_recente > 365*1)] = "Quente"
clientes_2014$segmento[which(clientes_2014$compra_mais_recente <= 365)] = "Ativo"
clientes_2014$segmento[which(clientes_2014$segmento == "Quente" & clientes_2014$primeira_compra <= 365*2)] = "Novo Quente"
clientes_2014$segmento[which(clientes_2014$segmento == "Quente" & clientes_2014$media_compra < 100)] = "Quente - Valor Baixo"
clientes_2014$segmento[which(clientes_2014$segmento == "Quente" & clientes_2014$media_compra >= 100)] = "Quente - Valor Alto"
clientes_2014$segmento[which(clientes_2014$segmento == "Ativo" & clientes_2014$primeira_compra <= 365)] = "Novo Ativo"
clientes_2014$segmento[which(clientes_2014$segmento == "Ativo" & clientes_2014$media_compra < 100)] = "Ativo - Valor Baixo"
clientes_2014$segmento[which(clientes_2014$segmento == "Ativo" & clientes_2014$media_compra >= 100)] = "Ativo - Valor Alto"

clientes_2014$segmento = factor(x = clientes_2014$segmento, levels = c("Inativo", "Frio",
                                                                       "Quente - Valor Alto", "Quente - Valor Baixo", "Novo Quente",
                                                                       "Ativo - Valor Alto", "Ativo - Valor Baixo", "Novo Ativo"))


# Computando a matriz de transição
new_data = merge(x = clientes_2014, y = clientes_2015, by = "cliente_id", all.x = TRUE)
head(new_data)
transition = table(new_data$segmento.x, new_data$segmento.y)
print(transition)

# Divide cada linha pela soma
transition = transition / rowSums(transition)
print(transition)

# Usando a matriz de transição para fazer previsões

# Inicializa a matriz com o número de clientes em cada segmento hoje e após 10 períodos
segmentos = matrix(nrow = 8, ncol = 11)
segmentos[, 1] = table(clientes_2015$segmento)
colnames(segmentos) = 2015:2025
row.names(segmentos) = levels(clientes_2015$segmento)
print(segmentos)

# Computando para cada período
for (i in 2:11) {
   segmentos[, i] = segmentos[, i-1] %*% transition
}

# Plot - Clientes Inativos e Ativos - Valor Alto ao longo do tempo
barplot(segmentos[1, ])
barplot(segmentos[2, ])

# Todos os segmentos ao longo do tempo
print(round(segmentos))



#####------------ Life Time Value -----------------


# Faturamento anual por segmento
yearly_revenue = c(0, 0, 0, 0, 0, 323.57, 52.31, 79.17)

# Computando o faturamento por segmento
revenue_per_segment = yearly_revenue * segmentos
print(revenue_per_segment)

# Faturamento anual
yearly_revenue = colSums(revenue_per_segment)
print(round(yearly_revenue))
barplot(yearly_revenue)

# Faturamento acumulado
cumulated_revenue = cumsum(yearly_revenue)
print(round(cumulated_revenue))
barplot(cumulated_revenue)

# Criando um fator de desconto
discount_rate = 0.10
discount = 1 / ((1 + discount_rate) ^ ((1:11) - 1))
print(discount)

# Faturamento anual com fator de desconto
disc_yearly_revenue = yearly_revenue * discount
print(round(disc_yearly_revenue))
barplot(disc_yearly_revenue)
lines(yearly_revenue)

# Faturamento acumulado com fator de desconto
disc_cumulated_revenue = cumsum(disc_yearly_revenue)
print(round(disc_cumulated_revenue))
barplot(disc_cumulated_revenue)

# Quanto vale nossa base de clientes em 2025?
print(disc_cumulated_revenue[11] - yearly_revenue[1])







