# Modelagem Preditiva em R

# Obs: Caso tenha problemas com a acentuação, consulte este link:
# https://support.rstudio.com/hc/en-us/articles/200532197-Character-Encoding

# Configurando o diretório de trabalho
# Coloque entre aspas o diretório de trabalho que você está usando no seu computador
# Não use diretórios com espaço no nome
setwd("/home/aline/Projetos/2")
getwd()

# Análise de Crédito

# Uma instituição bancária deseja criar um modelo preditivo que seja capaz de prever se 
# um cliente deve ou não receber crédito. Foram coletados dados históricos de clientes do 
# banco que serão usados para a construção do modelo.

# Seu modelo deve simplesmente retornar um Sim/Não como resultado e nível de acurácia superior a 75%.

# Carga de dados
clientes <- read.csv("data/clientes.csv", header = TRUE, sep = ",")
head(clientes)
str(clientes)
summary(clientes)


#####-------------- Pré-Processamento -----------------
# Função para transformar os dados - Fatorização
to.factors <- function(df, variables){
  for (variable in variables){
    df[[variable]] <- as.factor(df[[variable]])
  }
  return(df)
}

# Função para Normalização (Escala)
scale.features <- function(df, variables){
  for (variable in variables){
    df[[variable]] <- scale(df[[variable]], center=T, scale=T)
  }
  return(df)
}

# Normalizando as variáveis
numeric.vars <- c("credit.duration.months", "age", "credit.amount")
clientes <- scale.features(clientes, numeric.vars)

# Coletando as variáveis que serão transformadas em Fator
categorical.vars <- c('credit.rating', 'account.balance', 'previous.credit.payment.status',
                      'credit.purpose', 'savings', 'employment.duration', 'installment.rate',
                      'marital.status', 'guarantor', 'residence.duration', 'current.assets',
                      'other.credits', 'apartment.type', 'bank.credits', 'occupation', 
                      'dependents', 'telephone', 'foreign.worker')

# Convertendo as demais variáveis para fator
clientes <- to.factors(df = clientes, variables = categorical.vars)
head(clientes)
str(clientes)

# Divisão em dados de treino e de teste em uma taxa de 60:40 
indexes <- sample(1:nrow(clientes), size = 0.6 * nrow(clientes))
train.data <- clientes[indexes,]
test.data <- clientes[-indexes,]


#####-------------- Feature Selection -----------------
install.packages("caret")
install.packages("randomForest")
library(caret)  
library(randomForest) 

# Função para seleção de variáveis
run.feature.selection <- function(num.iters = 20, feature.vars, class.var){
  set.seed(10)
  variable.sizes <- 1:10
  control <- rfeControl(functions = rfFuncs, 
                        method = "cv", 
                        verbose = FALSE, 
                        returnResamp = "all", 
                        number = num.iters)
  results.rfe <- rfe(x = feature.vars, 
                     y = class.var, 
                     sizes = variable.sizes, 
                     rfeControl = control)
  return(results.rfe)
}

# Executando a função
rfe.results <- run.feature.selection(feature.vars = train.data[,-1],  class.var = train.data[,1])

# Resultado
rfe.results


#####-------------- Análise Preditiva com Árvores de Decisão -----------------
install.packages("rpart")
install.packages("rpart.plot")
install.packages("e1071")
library(rpart)
library(caret) 
library(rpart.plot) 
library(ROCR) 
library(e1071)

# Separando atributos e variáveis preditoras
test.feature.vars <- test.data[,-1]
test.class.var <- test.data[,1]

# Construindo o modelo inicial com os dados de treino
formula.init <- "credit.rating ~ ."
formula.init <- as.formula(formula.init)
dt.model <- rpart(formula = formula.init, 
                  method = "class", 
                  data = train.data, 
                  control = rpart.control(minsplit = 20, cp = 0.05))

# Prevendo e avaliando o resultado
dt.predictions <- predict(dt.model, test.feature.vars, type = "class")
confusionMatrix(data = dt.predictions, reference = test.class.var, positive = "1")


# Seleção de variáveis
formula.init <- "credit.rating ~ ."
formula.init <- as.formula(formula.init)
control <- trainControl(method = "repeatedcv", number = 10, repeats = 2)
model <- train(formula.init, data = train.data, method = "rpart", trControl = control)
importance <- varImp(model, scale = FALSE)
plot(importance, cex.lab = 0.5)

## Construindo um modelo com as variáveis selecionadas
formula.new <- "credit.rating ~ account.balance + credit.amount + credit.duration.months + previous.credit.payment.status"
formula <- as.formula(formula.new)
dt.model.new <- rpart(formula = formula, 
                   method = "class",
                   data = train.data, 
                   control = rpart.control(minsplit = 20, cp = 0.05),
                   parms = list(prior = c(0.7, 0.3)))

# Previsões e Avaliação do Resultado
dt.predictions <- predict(dt.model, test.feature.vars, type = "class")
confusionMatrix(data = dt.predictions, reference = test.class.var, positive = "1")

# Visualizando detalhes do modelo
dt.model.best <- dt.model.new
print(dt.model.best)
par(mfrow=c(1,1))
prp(dt.model.best, type = 1, extra = 3, varlen = 0, faclen = 0)

# Plot do Modelo com Curva ROC
plot.roc.curve <- function(predictions, title.text){
  perf <- performance(predictions, "tpr", "fpr")
  plot(perf,col = "black",lty=1, lwd=2,
       main=title.text, cex.main=0.6, cex.lab=0.8,xaxs="i", yaxs="i")
  abline(0,1, col="red")
  auc <- performance(predictions,"auc")
  auc <- unlist(slot(auc, "y.values"))
  auc <- round(auc,2)
  legend(0.4,0.4,legend=c(paste0("AUC: ",auc)),cex=0.6,bty = "n",box.col = "white")
  
}

dt.predictions.best <- predict(dt.model.best, test.feature.vars, type="prob")
dt.prediction.values <- dt.predictions.best[,2]
predictions <- prediction(dt.prediction.values, test.class.var)
par(mfrow = c(1,2))
plot.roc.curve(predictions, title.text = "Curva ROC - Árvore de Decisão")



#####-------------- Análise Preditiva com Regressão Logística -----------------
install.packages("ROCR")
library(caret) 
library(ROCR) 

# Separa Atributos e Variável target
test.feature.vars <- test.data[,-1]
test.class.var <- test.data[,1]

# Construindo o modelo de regressão
formula.init <- "credit.rating ~ ."
formula.init <- as.formula(formula.init)
lr.model <- glm(formula = formula.init, data = train.data, family = "binomial")

# Visualizando o resultado
summary(lr.model)

# Previsão e avaliação
lr.predictions <- predict(lr.model, test.data, type = "response")
lr.predictions <- round(lr.predictions)
confusionMatrix(data = lr.predictions, reference = test.class.var, positive = '1')

# Seleção de variáveis método glm
formula <- "credit.rating ~ ."
formula <- as.formula(formula)
control <- trainControl(method="repeatedcv", number = 10, repeats = 2)
model <- train(formula, data = train.data, method = "glm", trControl = control)
importance <- varImp(model, scale = FALSE)
plot(importance)

# Construindo o modelo com variáveis selecionadas
formula.new <- "credit.rating ~ account.balance + credit.purpose + previous.credit.payment.status + savings + credit.duration.months"
formula.new <- as.formula(formula.new)
lr.model.new <- glm(formula = formula.new, data = train.data, family = "binomial")

# Visualizando o modelo
summary(lr.model.new)

# Previsões e avaliações
lr.predictions.new <- predict(lr.model.new, test.data, type = "response") 
lr.predictions.new <- round(lr.predictions.new)
confusionMatrix(data = lr.predictions.new, reference = test.class.var, positive = '1')

# Plot do Modelo com Curva ROC
plot.roc.curve <- function(predictions, title.text){
  perf <- performance(predictions, "tpr", "fpr")
  plot(perf,col="black",lty=1, lwd=2,
       main=title.text, cex.main=0.6, cex.lab=0.8,xaxs="i", yaxs="i")
  abline(0,1, col="red")
  auc <- performance(predictions,"auc")
  auc <- unlist(slot(auc, "y.values"))
  auc <- round(auc,2)
  legend(0.4,0.4,legend=c(paste0("AUC: ",auc)),cex=0.6,bty = "n",box.col = "white")
  
}

lr.model.best <- lr.model
lr.prediction.values <- predict(lr.model.best, test.feature.vars, type="response")
predictions <- prediction(lr.prediction.values, test.class.var)
par(mfrow=c(1,2))
plot.roc.curve(predictions, title.text = "Curva ROC - Regressão Logística")




#####-------------- Análise Preditiva com Redes Neurais -----------------
library(caret) 
library(ROCR) 

# Separa Atributos e Variável target
test.feature.vars <- test.data[,-1]
test.class.var <- test.data[,1]

# Transformação de dados
transformed.train <- train.data
transformed.test <- test.data

for (variable in categorical.vars){
  new.train.var <- make.names(train.data[[variable]])
  transformed.train[[variable]] <- new.train.var
  new.test.var <- make.names(test.data[[variable]])
  transformed.test[[variable]] <- new.test.var
}

transformed.train <- to.factors(df=transformed.train, variables=categorical.vars)
transformed.test <- to.factors(df=transformed.test, variables=categorical.vars)
transformed.test.feature.vars <- transformed.test[,-1]
transformed.test.class.var <- transformed.test[,1]

# Construindo o modelo com dados de treino
formula.init <- "credit.rating ~ ."
formula.init <- as.formula(formula.init)
nn.model <- train(formula.init, data = transformed.train, method = "nnet")

# Visualizando resultados do modelo
print(nn.model)

# Previsões e avaliação
nn.predictions <- predict(nn.model, transformed.test.feature.vars, type = "raw")
confusionMatrix(data = nn.predictions, reference = transformed.test.class.var, positive = "X1")

# Seleção de variáveis
formula.init <- "credit.rating ~ ."
formula.init <- as.formula(formula.init)
control <- trainControl(method="repeatedcv", number = 10, repeats = 2)
model <- train(formula.init, data = transformed.train, method = "nnet", trControl = control)
importance <- varImp(model, scale = FALSE)
plot(importance, cex.lab = 0.5)

# Construindo o modelo com as variáveis selecionadas
formula.new <- "credit.rating ~ account.balance + credit.purpose + savings + current.assets + foreign.worker + previous.credit.payment.status"
formula.new <- as.formula(formula.new)
nn.model.new <- train(formula.new, data = transformed.train, method = "nnet")

# Previsões e Avaliação
nn.predictions.new <- predict(nn.model.new, transformed.test.feature.vars, type = "raw")
confusionMatrix(data = nn.predictions.new, reference = transformed.test.class.var, positive = "X1")

# Visualização de Hiperparâmetros
# plot(nn.model.new, cex.lab=0.5)

# Plot do Modelo com Curva ROC
plot.roc.curve <- function(predictions, title.text){
  perf <- performance(predictions, "tpr", "fpr")
  plot(perf,col="black",lty=1, lwd=2,
       main=title.text, cex.main=0.6, cex.lab=0.8,xaxs="i", yaxs="i")
  abline(0,1, col="red")
  auc <- performance(predictions,"auc")
  auc <- unlist(slot(auc, "y.values"))
  auc <- round(auc,2)
  legend(0.4,0.4,legend=c(paste0("AUC: ",auc)),cex=0.6,bty = "n",box.col = "white")
  
}

nn.model.best <- nn.model
nn.predictions.best <- predict(nn.model.best, transformed.test.feature.vars, type="prob")
nn.prediction.values <- nn.predictions.best[,2]
predictions <- prediction(nn.prediction.values, test.class.var)
par(mfrow=c(1,2))
plot.roc.curve(predictions, title.text="Curva ROC - Redes Neurais")



