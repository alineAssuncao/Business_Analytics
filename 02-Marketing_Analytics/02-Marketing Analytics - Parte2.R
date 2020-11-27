# Marketing Analytics em R
# Módulo 2 - Segmentação Estatística e Hierárquica de Clientes

# ***** Esta é a versão 4.0 deste script, atualizado em 01/04/2018 *****
# ***** Esse script pode ser executado nas versões 3.3.1, 3.3.2, 3.3.3, 3.4.0, 3.4.1 e 3.4.4 da linguagem R *****
# ***** Recomendamos a utilização da versão 3.4.4 da linguagem R *****

# Carregando os dados
setwd("/home/aline/Projetos/3")
getwd()

data = read.delim(file = 'data/compras.txt', header = FALSE, sep = '\t', dec = '.')

# Adicionando cabeçalho e adicionando uma coluna extra com o ano da compra
colnames(data) = c('cliente_id', 'valor_compra', 'data_compra')
data$data_compra = as.Date(data$data_compra, "%Y-%m-%d")
data$days_since  = as.numeric(difftime(time1 = "2016-01-01", time2 = data$data_compra, units = "days"))

# Resumo dos dados após modificação
head(data)
summary(data)

# Computando indicadores chave de Marketing com SQL
library(sqldf)

# Computando frequência, média de compras e days-since
clientes = sqldf("SELECT cliente_id,
                          MIN(days_since) AS 'days_since',
                          COUNT(*) AS 'frequencia',
                          AVG(valor_compra) AS 'media_compra'
                   FROM data GROUP BY 1")

# Explorando os dados
head(clientes)
summary(clientes)
hist(clientes$days_since)
hist(clientes$frequencia)
hist(clientes$media_compra)
hist(clientes$media_compra, breaks = 120)


#####---------------- Preparando e transformando os dados --------------------

# Copiando os dados em um novo dataframe
new_data = clientes

# Remove o cliente_id como variável e armazena como título para as linhas
head(new_data)
row.names(new_data) = new_data$cliente_id
new_data$cliente_id = NULL
head(new_data)

# Aplicando transformação baseada em log para o valor médio das compras
# Isso vai converter os dados para uma distribuição normal
new_data$media_compra = log(new_data$media_compra)
hist(new_data$media_compra)

# Padronização das variáveis
new_data = scale(new_data)
head(new_data)


#####---------------- Segmentação Hierárquica --------------------

# Obtendo uma amostra de 10%
sample = seq(1, 18417, by = 10)
head(sample)
clientes_sample = clientes[sample, ]
new_data_sample  = new_data[sample, ]

# Computação das métricas de distância nos dados padronizados
d = dist(new_data_sample)

# Clustering Hierárquico nas métricas de distância
?hclust
c = hclust(d, method = "ward.D2")

# Plot do Dendograma
plot(c)

# Reduz para 9 segmentos
members = cutree(c, k = 9)

# Tabela de frequência de 30 clientes
members[1:30]
table(members)

# Perfil de cada segmento
aggregate(clientes_sample[, 2:4], by = list(members), mean)










