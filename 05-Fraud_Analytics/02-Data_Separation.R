# Fraud Analytics - Separação dos Dados em diferentes amostras

# Obs: Caso tenha problemas com a acentuação, consulte este link:
# https://support.rstudio.com/hc/en-us/articles/200532197-Character-Encoding

# Configurando o diretório de trabalho
# Coloque entre aspas o diretório de trabalho que você está usando no seu computador
# Não use diretórios com espaço no nome
setwd("/home/aline/Projetos/6")
getwd()

# Carga dos dados
# dados = read.csv("dataset_source.csv")
# dim(dados)
# View(dados)

# Split dos dados
# ?floor
# amostra = floor(nrow(dados) * 0.8)         # 80% para treino e 20% para teste
# dados = dados[sample(nrow(dados)), ]       # amostragem
# treino = dados[1:amostra, ]                # dataset de treino
# teste  = dados[(amostra+1):nrow(dados), ]  # dataset de teste
# dim(treino)
# dim(teste)

# Carregando os dados de treino
treino = read.csv("dados_treino.csv", header = T)
treino$label = as.factor(treino$label)
summary(treino$label)

# Carregando os dados de teste e de validação
teste = read.csv("data/dados_teste.csv")
validate = read.csv("data/dados_validate.csv")

# Carregando as amostras - Gerando escala nos dados
# N = 1, 2, 3, 7
# card_scale_trans_N    = (90 / N x número de transações nos últimos N dias no cartão / número de transações nos últimos 90 dias no cartão)
# card_scale_amount_N   = (90 / N x valor total de transações nos últimos N dias no cartão / número de transações nos últimos 90 dias no cartão)
# merch_scale_trans_N   = (90 / N x número de transações nos últimos N dias no comerciante / número de transações nos últimos 90 dias no comerciante)
# merch_scale_amnount_N = (90 / N x valor total de transações nos últimos N dias no comerciante / quantidade total de transações nos últimos 90 dias no comerciante)
# card_scale_dup_N      = (100 x número de transações de mesma quantidade nos últimos N dias com cartão / número de transações nos últimos N dias no cartão)
# card_scale_dup_N      = (100 x número de transações de mesma quantidade nos últimos N dias no comerciante / número de transações nos últimos N dias no comerciante)
# card_scale_state_N    = (100 x número de transações no dia anterior com o mesmo estado / número de transações no dia anterior no cartão)

# Gerando amostras
amostra1 = read.csv("data/amostra_1.csv")
View(amostra1)
amostra3 = read.csv("data/amostra_3.csv")
amostra5 = read.csv("data/amostra_5.csv")
amostra7 = read.csv("data/amostra_7.csv")
amostra10 = read.csv("data/amostra_10.csv")

# Sumário
treino$label    = as.factor(treino$label)
teste$label     = as.factor(teste$label)
validate$label  = as.factor(validate$label)
amostra1$label  = as.factor(amostra1$label)
amostra3$label  = as.factor(amostra3$label)
amostra5$label  = as.factor(amostra5$label)
amostra7$label  = as.factor(amostra7$label)
amostra10$label = as.factor(amostra10$label)

# As razões entre não-fraude-fraude  são  1/1,  3/1,  5/1,  7/1  e 10/1
summary(treino$label)
summary(teste$label)
summary(amostra1$label)
summary(amostra3$label)
summary(amostra5$label)
summary(amostra7$label)
summary(amostra10$label)





