# Instagram Analytics

# Obs: Caso tenha problemas com a acentuação, consulte este link:
# https://support.rstudio.com/hc/en-us/articles/200532197-Character-Encoding

# Configurando o diretório de trabalho
# Coloque entre aspas o diretório de trabalho que você está usando no seu computador
# Não use diretórios com espaço no nome
setwd("Z:/Dropbox/DSA/Business-Analytics/R/Cap08/Instagram")
getwd()

# Pacotes
library(devtools)
library(RCurl)
library(httr)
library(rjson)

# Pacote instaR
# https://cran.r-project.org/web/packages/instaR/instaR.pdf
install_github("pablobarbera/instaR/instaR")
library(instaR)

# Dados para autenticação
app_name <- "DSA-app"
client_id <- "xxx"
client_secret <- "xxx"
scope = "public_content"

# Gerando token
token <- instaOAuth(client_id, client_secret, scope = "public_content")
print(token)
my_token <- token$credentials$access_token

# Salvando o token em disco
save(token, file = "instagram-token")
load(token)

# Acessando end-points
# https://www.instagram.com/developer/endpoints/users/
  
# Coletando info do usuário
user_info <- paste('https://api.instagram.com/v1/users/self/?access_token=', my_token, sep = "")
user_info_data <- getURL(user_info)
json_data_user <- fromJSON(user_info_data)
View(json_data_user)

# Coletando mídias mais recentes
recent_media <- paste('https://api.instagram.com/v1/users/self/media/recent/?access_token=', my_token, sep = "")
media_data <- getURL(recent_media)
json_data_media <- fromJSON(media_data)
print(json_data_media)

# Coletando likes
likes <- paste('https://api.instagram.com/v1/users/self/media/liked?access_token=', my_token, sep = "")
likes_data <- getURL(likes)
json_data_likes <- fromJSON(likes_data)
print(json_data_likes)


### PERMISSÃO SENDO AVALIADA
###################### Somente funciona em Apps com Permissões ###################### 
# Você precisa submeter sua app para aprovação pelo Instagram. Todos os detalhes na página da api.

# Coletando dados
?searchInstagram
data <- searchInstagram("DataScience", token, n = 5, folder = "DataScience")
head(data,2)
names(data)
head(data$comments_count)

# Obtendo fotos de uma localidade
data <- searchInstagram("DataScience", token, n = 10, lat = 13.1633, lng = 72.5456, folder = "DataScience")

# Download das 15 fotos mais atuais
instag <- getUserMedia("dsacademybr", token, n = 15, folder = "instagram")
names(instag)
head(instag)

# User profile
usr <- getUser("dsacademybr", token)
head(usr)
names(usr)

# Obtendo a lista de usuários seguidores
instaf <- getFollowers("instagram", token)
head(instaf,2)
names(instaf)
nrow(instaf)

# Lista de usuários que são seguidos
instaff <- getFollows("instagram", token)
head(instaff,3)
nrow(instaff)

# Obtendo comentários
comm <- getComments("1026058420312409485_25025320", token)
comm$text
tail(comm)
names(comm)
?getComments
class(comm)

# Obtendo a quantidade de vezes que uma hashtag é usada
tag1 <- getTagCount("trump", token)
tag1
tag2 <- getTagCount("bigdata", token)
tag2


###################### Exemplos usando arquivos CSV ###################### 
# Veremos como realizar análises com dados do Instagram sem a necessidade de conectar com a api.

## Preparando os dados
userAndTags <- read.csv("dataset1-hashtags.csv")
names(userAndTags)
head(userAndTags)

# Extraindo os usuários
users <- userAndTags$Users
users <- as.matrix(users)

# User media
usermedia <- read.csv("dataset2-usermedia.csv")
head(usermedia)
tags <- userAndTags$Hashtags
tags <- as.matrix(tags)
hashmedia <- read.csv("dataset3-hashmedia.csv")
head(hashmedia)

# Juntando os datasets
alldata <- rbind(usermedia, hashmedia)
head(alldata)

## Follows data
users <- userAndTags$Users
users <- as.matrix(users)
userfollows <- read.csv("dataset4-userfollows.csv")
View(userfollows)

## Consolidando user profile

users <- userAndTags$Users
users <- as.matrix(users)
userprofiles <- read.csv("dataset5-userprofiles.csv")
View(userprofiles)

## Quem possui mais seguidores
mostfollowed <- userprofiles[with(userprofiles, order(-followed_by)), ]
head(mostfollowed$full_name, 15)

## Quem segue mais?
mostfollows <- userprofiles[with(userprofiles, order(-follows)), ]
head(mostfollows$full_name, 15)

# Quem publica mais?
mostmedias <- userprofiles[with(userprofiles, order(-media_count)), ]
head(mostmedias$full_name, 15)

# Top users
userprofiles$overallmetric <- ((userprofiles$media_count/max(userprofiles$media_count)) + (userprofiles$followed_by/max(userprofiles$followed_by)) +(userprofiles$followed_by/max(userprofiles$followed_by)))*100
overallmet <- userprofiles[with(userprofiles, order(-overallmetric)), ]
head(overallmet$full_name, 15)
View(overallmet)

# Mais comentados
head(alldata)
mostcomm <- alldata[with(alldata, order(-comments_count)), ]
View(mostcomm)

# Com mais likes
head(alldata)
mostlikes <- alldata[with(alldata, order(-likes_count)), ]
View(mostlikes)

## Todas as localidades
library(sqldf)
names(alldata)
allloc <- sqldf("select distinct location_name from alldata")
allloc <- na.omit(allloc)
head(allloc, 20)
nrow(allloc)

# localidades com mais likes
loclikes <- sqldf("select location_name, sum(likes_count) as totlikes from alldata group by location_name")
loc <- loclikes[with(loclikes, order(-totlikes)), ]
loc <- na.omit(loc)
head(loc, 25)

# Localidade mais falada
loccomments <- sqldf("select location_name, sum(comments_count) as totcomm from alldata group by location_name")
loccomm <- loccomments[with(loccomments, order(-totcomm)), ]
loccomm <- na.omit(loccomm)
head(loccomm, 15)

# Localidade que ocorre com mais frequência 
locations <- sqldf("select location_name, count(location_id) as locid from alldata group by location_name")
location <- locations[with(locations, order(-locid)), ]
location <- na.omit(location)
head(location,5)

# Com mais likes, por usuário
names(alldata)
userlikes <- sqldf("select username, sum(likes_count) as totlikes from alldata group by username")
user <- userlikes[with(userlikes, order(-totlikes)), ]
user <- na.omit(user)
head(user, 25)

# Wordcloud
library(wordcloud)
library(tm)

# Limpeza nos dados
options(warn=-1)
words <- strsplit(as.character(alldata$caption), " ")
words <- lapply(words, function(x) x[grep("^[A-Za-z0-9]+$", x)])
words <- unlist(words)
words <- tolower(words)
words <- words[-grep("^[rm]t$", words)]

# Removendo stop words
stopWords <- stopwords("en")
"%!in%" <- function(x,table) match(x,table, nomatch = 0) == 0
words <- words[words %!in% stopWords]

# Criando um objeto com a wordcloud
allwords <- as.data.frame(table(words))
wc <- wordcloud(allwords$words, allwords$Freq, 
                random.order = FALSE, 
                min.freq = 5, 
                colors = brewer.pal(2, "Dark2"))

# Gravando a wordcloud
dev.copy(png,filename = "wordcloud.png", width = 600, height = 875);
dev.off ();


## Clustering
??fpc
library(fpc)

head(alldata)
data <- alldata

# Extraindo algumas colunas
cdata <- data.frame(data$type, data$comments_count, data$likes_count, data$user_id)

# Nomeando as colunas
colnames(cdata) <- c("type","comments","likes","user_id")

# Convertendo para integer (alguns algoritmos de clusters somente aceitam números como entrada)
cdata$user_id <- as.integer(cdata$user_id)
cdata$type <- as.integer(cdata$type)

# Visualizando os dados
View(data)
View(cdata)

# Estimando o número de clusters (automático)
?pamk
clusters <- pamk(cdata)
n <- clusters$nc
n

# Estimando o número de clusters (manual)
# Calculando a soma dos erros quadrados 
wss <- (nrow(cdata)-1)*sum(apply(cdata,2,var))

# Buscando a soma dos erros quadrados dentro dos grupos
for (i in 2:25) wss[i] <- sum(kmeans(cdata, centers=i)$withinss)

# Plot dos clusters
# Logicamente, à medida que o número de clusters aumenta, a soma dos erros quadrados reduz. 
# Se houver n objetos em um conjunto de dados, então n clusters resultaria em erro 0, mas idealmente 
# precisamos parar em algum ponto. De acordo com as teorias, a taxa de diminuição na soma dos erros irá 
# cair de repente em um ponto e que deve ser considerado como o número ideal de clusters. 
# De acordo com o gráfico a seguir, o número ideal de cluster é 4.
plot(1:25, wss, type = "b", xlab = "Número de clusters", ylab = "Soma dos Quadrados Dentro dos grupos")

# Salvando o plot
dev.copy(png,filename = "clustering.png", width = 600, height = 875)
dev.off ();

## K-Means

# K-Means Cluster Analysis
?kmeans
fit <- kmeans(cdata, 4)
fit$cluster

# Número de elementos em cada cluster
table(fit$cluster)

# Médias dos clusters
aggregate(cdata, by = list(fit$cluster), FUN = mean)

# Observações em cada cluster
resultado <- cbind(cdata, clusterNum = fit$cluster)
View(resultado)

# Plot do cluster
library(fpc)
plotcluster(cdata, fit$cluster)
dev.copy(png, filename = "clusterPlot.png", width = 600, height = 875);
dev.off ();




## Sistema de Recomendação Baseado em Usuário
## Projeto 5 - Construindo um Sistema de Recomendação
library(data.table)

# Carregando o dataset com seguidores
userfollows <- read.csv("dataset4-userfollows.csv")
head(userfollows)
names(userfollows)

# Temos a variável precedente no conjunto de dados. Para construir o mecanismo de recomendação e fornecer 
# recomendações aos usuários, precisamos apenas de duas colunas. Portanto, selecionamos essas duas colunas 
# usando a função data.frame:
fdata <- data.frame(userfollows$users.i..1., userfollows$username)
colnames(fdata) <- c("user","follows")
head(fdata)

# Pivot dos dados
# Agora, temos de manipular o conjunto de dados de tal forma que os usuários se tornem a coluna e 
# os usuários que seguem, se tornem as linhas. Assim, torna-se fácil calcular a correlação entre os usuários. 
# Para girar os dados, precisamos usar a função dcast.data.table, que requer o pacote data.table.
pivoting <- data.table(fdata)
pivotdata <- dcast.data.table(pivoting, follows ~ user, fun.aggregate = length, value.var = "user")
write.csv(pivotdata, "dataset5-pivot-follows-temp.csv")

# Após deletar a coluna de índice e o usuário nulo

# Lendo os dados
data <- read.csv("dataset5-pivot-follows.csv")
head(data)
colnames(data)

# Removendo a coluna user
data.ubs <- (data[,!(names(data) %in% c("users"))])

# Função que calcula a distância entre 2 vetores
# Podemos calcular a similaridade dos usuários usando diferentes métodos. 
# Em nosso caso, usaremos a técnica de similaridade de coseno para obter a pontuação de similaridade 
# para todos os pares de usuários. Em nosso conjunto de dados, zero significa que o usuário não está seguindo. 
# Se considerarmos essas linhas com zero, enquanto calculamos a similaridade usando correlação ou qualquer 
# outra técnica, vamos acabar com uma saída tendenciosa que está longe da realidade. 
# Assim, ao calcular a pontuação de similaridade, consideraremos somente as linhas não nulas. 
# A seguinte função calcula a similaridade entre os usuários usando o método de similaridade de coseno. 
getCosine <- function(x,y) 
{
  dat <- cbind(x,y)
  f <- as.data.frame(dat)
  # Remove as linhas com zeros
  datn<- f[-which(rowSums(f==0)>0),]
  if(nrow(datn) > 2)
  {
    this.cosine <- sum(x*y) / (sqrt(sum(x*x)) * sqrt(sum(y*y)))
  }
  else
  {
    this.cosine <- 0
  }
  return(this.cosine)
}

# Agora, precisamos construir uma matriz de similaridade que nos dirá como os usuários são 
# semelhantes entre si. Antes de computar a similaridade, vamos construir uma matriz vazia que pode 
# ser usada para armazenar a similaridade:
data.ubs.similarity <- matrix(NA, 
                              nrow = ncol(data.ubs), 
                              ncol = ncol(data.ubs), 
                              dimnames = list(colnames(data.ubs),
                                              colnames(data.ubs)))

# Aplicando o cálculo de similaridade a todas as colunas
# Agora podemos começar a substituir as células vazias na matriz de similaridade com a pontuação de 
# similaridade real. No caso da similaridade de coseno, o intervalo será de -1 a + 1. 
# O seguinte loop ajudará a calcular a similaridade entre todos os usuários. 
# Se não houver dados suficientes para calcular a similaridade de acordo com nossa função, 
# ela retornará zero. A instrução print no seguinte loop nos ajudará a entender o progresso do loop. 
# Dependendo do conjunto de dados, o tempo necessário varia. 
for(i in 1:ncol(data.ubs)) {
  # Loop em todas as colunas
  for(j in 1:ncol(data.ubs)) {
    # Calcula a similaridade e alimenta o dataframe
    data.ubs.similarity[i,j] <- getCosine(as.matrix(data.ubs[i]),as.matrix(data.ubs[j]))
  }
  print(i)
}

# Converte a matriz de similaridade em dataframe
data.ubs.similarity <- as.data.frame(data.ubs.similarity)

# Replace NA com 0
data.ubs.similarity[is.na(data.ubs.similarity)] <- 0
head(data.ubs.similarity)

# Obtendo os 10 vizinhos de cada usuário
data.neighbours <- matrix(NA, 
                          nrow = ncol(data.ubs.similarity), 
                          ncol = 11, 
                          dimnames = list(colnames(data.ubs.similarity)))

# Gerando as recomendações
for(i in 1:ncol(data.ubs)) 
{
  # Evitando valores zero
  n <- length(data.ubs.similarity[,i])
  thres <- sort(data.ubs.similarity[,i],partial=n-10)[n-10]
  if(thres > 0.020)
  {
    # Selecionando as recomendações Top 10
    data.neighbours[i,] <- (t(head(n=11,rownames(data.ubs.similarity[order(data.ubs.similarity[,i],decreasing=TRUE),][i]))))
  }
  else
  {
    data.neighbours[i,] <- ""
  }
}

# Visualizando as recomendações
# No código anterior, pegamos um usuário de cada vez e, em seguida, classificamos a pontuação de similaridade 
# do usuário com todos os outros usuários, de modo que o par com a maior similaridade venha primeiro. 
# Então, paramos apenas filtrando os 10 primeiros para cada um dos usuários. Isso é recomendado para nós. 
# Podemos ver as recomendações dadas para os usuários::
View(data.neighbours)

# Gravando as recomendações
write.csv(data.neighbours, "Recomendacoes.csv")

