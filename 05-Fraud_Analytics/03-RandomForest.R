# Fraud Analytics - RandomForest

# Modelos com RandomForest, SVM, Redes Neurais, CART, Boosted Tree e KNN, são modelos não-lineares mais sofisticados. 
# Normalmente esses modelos apresentam um desempenho ligeiramente melhor do que a regressão logística (modelo linear), 
# por exemplo.

# A experiência mostra que a formação de Redes Neurais e Modelos de randomForest são, geralmente, mais eficientes 
# quando as variáveis numéricas independentes são escaladas ou normalizadas, de modo que suas magnitudes 
# são relativamente semelhantes. 

# A normalização também ajuda o SVM a funcionar melhor, já que todos os recursos têm aproximadamente a mesma 
# magnitude (já que não assumimos que alguns recursos são muito mais importantes do que outros). 

# Por esta razão, nós dimensionamos os dados que alimentamos nos modelos e colocamos todos na mesma escala,
# conforme datasets disponibilizados em anexo no item anterior. 

# Usaremos RandomForest neste script e Redes Neurais no Script seguinte.

# Diretório de trabalho
setwd("/home/aline/Projetos/6")

# Pacotes
library(randomForest) 
library(dplyr)
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

#------------------------ Como Calcular o FDR @3% -------------------------------------------------

# Para detecção de fraude, bom desempenho significa:
# Alta taxa de detecção (verdadeiros positivos), ou seja, quantos casos de fraude podem ser detectados corretamente.
# Baixa taxa de falsos positivos, isto é, com que frequência um caso de não fraude é falsamente detectado como fraude.

# Nós calculamos taxa de detecção de fraude (Fraud Detection Rate - FDR) para cada modelo, 
# a fim de saber qual deles executa melhor. Depois de executar cada modelo, obtemos uma probabilidade, 
# que usamos como pontuação, para cada registro.

# Nós classificamos os registros por probabilidade do maior para o menor e escolhemos top 3% (FDR @3%).

# Fórmula do FDR @3% = (label = 1 @3%) / (label = 1 @treino/Teste)

# Aplicamos o mesmo método a todos os modelos e comparamos nossos modelos com base em seus FDR @ 3%, 
# e fizemos comparações separadas para nossos modelos com RandomForest e Redes Neurais 
# (outros modelos poderiam ser usados).

# Os modelos não-lineares seguem o padrão esperado, com treinamento e teste praticamente o mesmo e validação 
# ligeiramente menor. O próximo passo é selecionar o modelo não-linear com melhor desempenho.

# O Modelo com RandomForest apresentou resultados superiores ao modelo com Redes Neurais.

# FDR @3% é apenas um aspecto de testar a eficácia dos modelos. 
# Para fazer mais melhorias, podemos estudar mais sobre os registros que são rotulados como fraude, 
# tentando encontrar o padrão oculto por trás deles e para chegar a melhores variáveis para teste de modelo.


#------------------------ Random Forest - Modelo 1 -------------------------------------------------
# Teste = 0.321
# Validação = 0.219

# Modelo 1 - amostra1
rf.fit = randomForest(label~., data = amostra1[,-1], mtry = 5, ntree = 1000, importance = TRUE) 
table(rf.fit$predicted)
table(amostra1$label)

# Teste
?predict
rf.preds = predict(rf.fit, newdata = teste)
?prediction
rf.pred = prediction(rf.preds, teste$label) 
a = data.frame(rf.preds, rf.pred@labels)
head(a)
colnames(a) = c("prob","label")
b1 = a %>% arrange(desc(prob)) %>% head(round(nrow(a)*0.03)) # Calcula do FDR @3%
head(b1)
nrow(b1[b1$label==1,])/nrow(teste[teste$label==1,])          # Calcula do FDR @3%
write.csv(a,"amostra1_predict.csv")

# Validação
rf.valids = predict(rf.fit, newdata = validate)
rf.valid = prediction(rf.valids, validate$label) 
c = data.frame(rf.valids, rf.valid@labels)
colnames(c) = c("prob","label")
d1 = c %>% arrange(desc(prob)) %>% head(round(nrow(c)*0.03))  # Calcula do FDR @3%
nrow(d1[d1$label==1,])/nrow(validate[validate$label==1,])     # Calcula do FDR @3%
write.csv(c,"amostra1_validate.csv")

#------------------------ Random Forest - Modelo 2 -------------------------------------------------
# Teste = 0.374
# Validação = 0.204

# Modelo 2 - amostra3
rf.fit = randomForest(label~., data = amostra3[,-1], mtry = 5, ntree = 1000, importance = TRUE) 
table(rf.fit$predicted)
table(amostra3$label)

# Teste
rf.preds = predict(rf.fit, newdata = teste)
rf.pred = prediction(rf.preds, teste$label)
a = data.frame(rf.preds, rf.pred@labels)
colnames(a) = c("prob","label")
b3 = a %>% arrange(desc(prob)) %>% head(round(nrow(a)*0.03))
nrow(b3[b3$label==1,])/nrow(teste[teste$label==1,])
write.csv(a,"amostra3_predict.csv")

# Validação
rf.valids = predict(rf.fit, newdata = validate)
rf.valid = prediction(rf.valids, validate$label) 
c = data.frame(rf.valids, rf.valid@labels)
colnames(c) = c("prob","label")
d3 = c %>% arrange(desc(prob)) %>% head(round(nrow(c)*0.03))
nrow(d3[d3$label==1,])/nrow(validate[validate$label==1,])
write.csv(c,"amostra3_validate.csv")

#------------------------ Random Forest - Modelo 3 -----------------------------------------------
# Teste = 0.419
# Validação = 0.231

# Modelo 3 - amostra5
rf.fit = randomForest(label~., data = amostra5[,-1], mtry = 5, ntree = 1000, importance = TRUE) 
table(rf.fit$predicted)
table(amostra5$label)

# Teste
rf.preds = predict(rf.fit, newdata = teste)
rf.pred = prediction(rf.preds, teste$label)
a = data.frame(rf.preds, rf.pred@labels)
colnames(a) = c("prob","label")
b5 = a %>% arrange(desc(prob)) %>% head(round(nrow(a)*0.03))
nrow(b5[b5$label==1,])/nrow(teste[teste$label==1,])
write.csv(a,"amostra5_predict.csv")

# Validação
rf.valids = predict(rf.fit, newdata = validate) 
rf.valid = prediction(rf.valids, validate$label) 
c = data.frame(rf.valids,rf.valid@labels)
colnames(c) = c("prob","label")
d5 = c %>% arrange(desc(prob)) %>% head(round(nrow(c)*0.03))
nrow(d5[d5$label==1,])/nrow(validate[validate$label==1,])
write.csv(c,"amostra5_validate.csv")

#------------------------ Random Forest - Modelo 4 ------------------------------------------------
# Teste = 0.423
# Validação = 0.238

# Modelo 4 - amostra7
rf.fit = randomForest(label~.,data = amostra7[,-1], mtry = 5, ntree = 1000, importance = TRUE) 
table(rf.fit$predicted)
table(amostra7$label)

# Teste
rf.preds = predict(rf.fit, newdata = teste)
rf.pred = prediction(rf.preds, teste$label)
a = data.frame(rf.preds, rf.pred@labels)
colnames(a) = c("prob","label")
b7 = a %>% arrange(desc(prob)) %>% head(round(nrow(a)*0.03))
nrow(b7[b7$label==1,])/nrow(teste[teste$label==1,])
write.csv(a,"amostra7_predict.csv")

# b7_10 = a %>% arrange(desc(prob)) %>% head(round(nrow(a)*0.1))
# nrow(b7_10[b7_10$label==1,])/nrow(teste[teste$label==1,])
# b7_20 = a %>% arrange(desc(prob)) %>% head(round(nrow(a)*0.2))
# nrow(b7_20[b7_20$label==1,])/nrow(teste[teste$label==1,])

# Validação
rf.valids = predict(rf.fit, newdata = validate)
rf.valid = prediction(rf.valids, validate$label) 
c = data.frame(rf.valids, rf.valid@labels)
colnames(c) = c("prob","label")
d7 = c %>% arrange(desc(prob)) %>% head(round(nrow(c)*0.03))
nrow(d7[d7$label==1,])/nrow(validate[validate$label==1,])
write.csv(c,"amostra7_validate.csv")

# d7_10 = c %>% arrange(desc(prob)) %>% head(round(nrow(c)*0.1))
# nrow(d7_10[d7_10$label==1,])/nrow(validate[validate$label==1,])
# vd7_20 = c %>% arrange(desc(prob)) %>% head(round(nrow(c)*0.2))
# nrow(d7_20[d7_20$label==1,])/nrow(validate[validate$label==1,])

#------------------------ Random Forest - Modelo 5 -----------------------------------------------
# Teste = 0.434
# Validação = 0.245

# Modelo 5 - amostra10
rf.fit = randomForest(label~.,data = amostra10[,-1], mtry = 5, ntree = 1000, importance = TRUE) 
table(rf.fit$predicted)
table(amostra10$label)

# Teste
rf.preds = predict(rf.fit, newdata = teste)
rf.pred = prediction(rf.preds, teste$label)
a = data.frame(rf.preds, rf.pred@labels)
colnames(a) = c("prob","label")
b10 = a %>% arrange(desc(prob)) %>% head(round(nrow(a)*0.03))
nrow(b10[b10$label==1,])/nrow(teste[teste$label==1,])
write.csv(a,"amostra10_predict.csv")

# Validação
rf.valids = predict(rf.fit, newdata = validate)
rf.valid = prediction(rf.valids, validate$label) 
c = data.frame(rf.valids, rf.valid@labels)
colnames(c) = c("prob","label")
d10 = c %>% arrange(desc(prob)) %>% head(round(nrow(c)*0.03))
nrow(d10[d10$label==1,])/nrow(validate[validate$label==1,])
write.csv(c,"amostra10_validate.csv")

#------------------------ Random Forest - Treino ----------------------------------------------------
# Teste = 0.427
# Validação = 0.271

# Modelo 6 - dados de treino
rf.fit = randomForest(label~., data = treino[,-1], mtry = 5, ntree = 1000, importance = TRUE) 
table(rf.fit$predicted)
table(treino$label)

# Teste
rf.preds = predict(rf.fit, newdata = teste)
rf.pred = prediction(rf.preds, teste$label)
a = data.frame(rf.preds, rf.pred@labels)
colnames(a) = c("prob","label")
b = a %>% arrange(desc(prob)) %>% head(round(nrow(a)*0.03))
nrow(b[b$label==1,])/nrow(teste[teste$label==1,])
write.csv(a,"treino_predict.csv")

# Validação
rf.valids = predict(rf.fit, newdata = validate)
rf.valid = prediction(rf.valids, validate$label) 
c = data.frame(rf.valids,rf.valid@labels)
colnames(c) = c("prob","label")
d = c %>% arrange(desc(prob)) %>% head(round(nrow(c)*0.03))
nrow(d[d$label==1,])/nrow(validate[validate$label==1,])
write.csv(c,"treino_validate.csv")




