# Marketing Analytics em R
# Módulo 3 - Segmentação Gerencial

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

# Computando frequência, média de compras e days-since
clientes_2015 = sqldf("SELECT cliente_id,
                               MIN(days_since) AS 'compra_mais_recente',
                               MAX(days_since) AS 'primeira_compra',
                               COUNT(*) AS 'frequencia',
                               AVG(valor_compra) AS 'media_compra'
                        FROM data GROUP BY 1")

# Explore the data
head(clientes_2015)
summary(clientes_2015)
hist(clientes_2015$compra_mais_recente)
hist(clientes_2015$frequencia)
hist(clientes_2015$media_compra)
hist(clientes_2015$media_compra, breaks = 100)


#####-------------------- Segmentação Gerencial ---------------------------

# Solução simples de 2 segmentos baseada nas compras mais recentes
# NA significa que ainda não atribuímos nenhum segmento
clientes_2015$segmento = ifelse(test = clientes_2015$compra_mais_recente > 365*3, yes = "Inativo", no = "NA")
table(clientes_2015$segmento)
aggregate(x = clientes_2015[, 2:5], by = list(clientes_2015$segmento), mean)

# Solução mais complexa de 3 segmentos baseada nas compras mais recentes
clientes_2015$segmento = ifelse(test = clientes_2015$compra_mais_recente > 365*3,
                           yes = "Inativo",
                           no = ifelse(test = clientes_2015$compra_mais_recente > 365*2,
                                       yes = "Frio",
                                       no = "NA"))

table(clientes_2015$segmento)
aggregate(x = clientes_2015[, 2:5], by = list(clientes_2015$segment), mean)

# Solução simples de 2 segmentos usando which
?which
clientes_2015$segmento = "NA"
clientes_2015$segmento[which(clientes_2015$compra_mais_recente > 365*3)] = "Inativo"
table(clientes_2015$segmento)
aggregate(x = clientes_2015[, 2:5], by = list(clientes_2015$segmento), mean)

# Solução mais complexa de 4 segmentos usando which
clientes_2015$segmento = "NA"
clientes_2015$segmento[which(clientes_2015$compra_mais_recente > 365*3)] = "Inativo"
clientes_2015$segmento[which(clientes_2015$compra_mais_recente <= 365*3 & clientes_2015$compra_mais_recente > 365*2)] = "Frio"
clientes_2015$segmento[which(clientes_2015$compra_mais_recente <= 365*2 & clientes_2015$compra_mais_recente > 365*1)] = "Quente"
clientes_2015$segmento[which(clientes_2015$compra_mais_recente <= 365)] = "Ativo"
table(clientes_2015$segmento)
aggregate(x = clientes_2015[, 2:5], by = list(clientes_2015$segmento), mean)

# Solução completa de segmentação
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
table(clientes_2015$segmento)
aggregate(x = clientes_2015[, 2:5], by = list(clientes_2015$segmento), mean)

# Reordenando os fatores
clientes_2015$segmento = factor(x = clientes_2015$segmento, levels = c("Inativo", "Frio",
                                                             "Quente - Valor Alto", "Quente - Valor Baixo", "Novo Quente",
                                                             "Ativo - Valor Alto", "Ativo - Valor Baixo", "Novo Ativo"))

table(clientes_2015$segmento)
aggregate(x = clientes_2015[, 2:5], by = list(clientes_2015$segmento), mean)



#####------------------- Computando a quantidade de faturamento gerado por segmento -----------------------

# Extraindo os dados
faturamento_2015 = sqldf("SELECT cliente_id, SUM(valor_compra) AS 'faturamento_2015'
                      FROM data
                      WHERE ano_compra = 2015
                      GROUP BY 1")
summary(faturamento_2015)


# Merge do dataset de clientes por segmento com dataset de faturamento
actual = merge(clientes_2015, faturamento_2015, all.x = TRUE)
head(actual)
actual$faturamento_2015[is.na(actual$faturamento_2015)] = 0
head(actual)

# Média do faturamento por cliente e por segmento em 2015
r = aggregate(x = actual$faturamento_2015, by = list(clientes_2015$segmento), mean)
print(r)

# Organiza o resultado
r = r[order(r$x, decreasing = TRUE), ]
print(r)
barplot(r$x, names.arg = r$Group.1)





