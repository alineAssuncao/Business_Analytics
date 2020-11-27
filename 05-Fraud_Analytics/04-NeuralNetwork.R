# Fraud Analytics - Redes Neurais

# Obs: Caso tenha problemas com a acentuação, consulte este link:
# https://support.rstudio.com/hc/en-us/articles/200532197-Character-Encoding

# Configurando o diretório de trabalho
# Coloque entre aspas o diretório de trabalho que você está usando no seu computador
# Não use diretórios com espaço no nome
setwd("/home/aline/Projetos/6")
getwd()

# Pacotes
library(dplyr) 
library(nnet) 
library(rpart) 
library(ROCR) 

# Carregando os datasets
treino = read.csv("data/dados_treino.csv", header = T)
teste = read.csv("data/dados_teste.csv")
validate = read.csv("data/dados_validate.csv")

# Carregando as amostras
amostra1 = read.csv("data/amostra_1.csv")
amostra3 = read.csv("data/amostra_3.csv")
amostra5 = read.csv("data/amostra_5.csv")
amostra7 = read.csv("data/amostra_7.csv")
amostra10 = read.csv("data/amostra_10.csv")

#------------------------ Redes Neurais - Modelo 1 -----------------------------------------------------
# Teste = 0.172
# Validação = 0.083

# Modelo 1 - amostra1
nnet.fit = nnet(label~.-Record, data = amostra1, size = 20, maxit = 10000, decay = .001) 

# Teste
nnet.preds = predict(nnet.fit, newdata = teste, type = "raw") 
nnet.pred = prediction(nnet.preds, teste$label) 
a = data.frame(nnet.preds, nnet.pred@labels)
colnames(a) = c("prob","label")
b1 = a %>% arrange(desc(prob)) %>% head(round(nrow(a)*0.03))
head(b1)
tail(b1)
nrow(b1[b1$label==1,])/nrow(teste[teste$label==1,])
write.csv(a,"data/amostra1_nn_predict.csv")

# Validação
nnet.preds = predict(nnet.fit, newdata = validate, type = "raw") 
nnet.pred = prediction(nnet.preds, validate$label) 
c = data.frame(nnet.preds, nnet.pred@labels)
colnames(a) = c("prob","label")
d1 = a %>% arrange(desc(prob)) %>% head(round(nrow(c)*0.03))
nrow(d1[d1$label==1,])/nrow(validate[validate$label==1,])
write.csv(c,"data/amostra1_nn_validate.csv")

#------------------------ Redes Neurais - Modelo 2 -----------------------------------------------------
# Teste = 0.238
# Validação = 0.124

# Modelo 2 - amostra3
nnet.fit = nnet(label~.-Record, data = amostra3, size = 20, maxit = 10000, decay = .001) 

# Teste
nnet.preds = predict(nnet.fit, newdata = teste, type = "raw") 
nnet.pred = prediction(nnet.preds, teste$label) 
a = data.frame(nnet.preds, nnet.pred@labels)
colnames(a) = c("prob","label")
b3 = a %>% arrange(desc(prob)) %>% head(round(nrow(a)*0.03))
nrow(b3[b3$label==1,])/nrow(teste[teste$label==1,])
write.csv(a,"data/amostra3_nn_predict.csv")

# Validação
nnet.preds = predict(nnet.fit, newdata = validate, type = "raw") 
nnet.pred = prediction(nnet.preds, validate$label) 
c = data.frame(nnet.preds, nnet.pred@labels)
colnames(a) = c("prob","label")
d3 = a %>% arrange(desc(prob)) %>% head(round(nrow(c)*0.03))
nrow(d3[d3$label==1,])/nrow(validate[validate$label==1,])
write.csv(c,"data/amostra3_nn_validate.csv")

#------------------------ Redes Neurais - Modelo 3 -----------------------------------------------------
# Teste = 0.134
# Validação = 0.107

# Modelo 3 - amostra7
nnet.fit = nnet(label~.-Record, data = amostra7, size = 20, maxit = 10000, decay = .001) 

# Teste
nnet.preds = predict(nnet.fit, newdata = teste, type = "raw") 
nnet.pred = prediction(nnet.preds, teste$label) 
a = data.frame(nnet.preds, nnet.pred@labels)
colnames(a) = c("prob","label")
b7 = a %>% arrange(desc(prob)) %>% head(round(nrow(a)*0.03))
nrow(b7[b7$label==1,])/nrow(teste[teste$label==1,])
write.csv(a,"data/amostra7_nn_predict.csv")

# Validação
nnet.preds = predict(nnet.fit, newdata = validate, type = "raw") 
nnet.pred = prediction(nnet.preds, validate$label) 
c = data.frame(nnet.preds,nnet.pred@labels)
colnames(a) = c("prob","label")
d7 = a %>% arrange(desc(prob)) %>% head(round(nrow(c)*0.03))
nrow(d7[d7$label==1,])/nrow(validate[validate$label==1,])
write.csv(c,"data/amostra7_nn_validate.csv")

#------------------------ Redes Neurais - Modelo 4 -----------------------------------------------------
# Teste = 0.280
# Validação = 0.118

# Modelo 4 - dados de treino
nnet.fit = nnet(label~.-Record, data = treino, size = 20, maxit = 10000, decay = .001) 

# Teste
nnet.preds = predict(nnet.fit, newdata = teste, type = "raw") 
nnet.pred = prediction(nnet.preds, teste$label) 
a = data.frame(nnet.preds,nnet.pred@labels)
colnames(a) = c("prob","label")
b = a %>% arrange(desc(prob)) %>% head(round(nrow(a)*0.03))
nrow(b[b$label==1,])/nrow(teste[teste$label==1,])
write.csv(a,"data/treino_nn_predict.csv")


# Validação
nnet.preds = predict(nnet.fit, newdata = validate, type = "raw") 
nnet.pred = prediction(nnet.preds, validate$label) 
c = data.frame(nnet.preds, nnet.pred@labels)
colnames(a) = c("prob","label")
d = a %>% arrange(desc(prob)) %>% head(round(nrow(c)*0.03))
nrow(d[d$label==1,])/nrow(validate[validate$label==1,])
write.csv(c,"data/treino_nn_validate.csv")




