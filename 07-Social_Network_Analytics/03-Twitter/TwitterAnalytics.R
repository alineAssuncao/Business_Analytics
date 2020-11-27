# Social Network Analytics - Twitter Analytics
# Analisando dados do Twitter

# Procedimento para solicitar seus tweets
# https://support.twitter.com/articles/20170330

# Obs: Caso tenha problemas com a acentuação, consulte este link:
# https://support.rstudio.com/hc/en-us/articles/200532197-Character-Encoding

# Configurando o diretório de trabalho
# Coloque entre aspas o diretório de trabalho que você está usando no seu computador
# Não use diretórios com espaço no nome
setwd("Z:/Dropbox/DSA/Business-Analytics/R/Cap08/Twitter")
getwd()

# Instalando os pacotes
install.packages("ggplot2")
install.packages("lubridate")
install.packages("scales")

# Carregando os pacotes
library(ggplot2)
library(lubridate)
library(scales)

# Carregando o dataset
tweets <- read.csv("tweets.csv", stringsAsFactors = FALSE)
head(tweets)
class(tweets)

# Ajustando a data
tweets$timestamp <- ymd_hms(tweets$timestamp)
tweets$timestamp <- with_tz(tweets$timestamp, "America/Sao_Paulo")
Sys.timezone(location = TRUE)

# Histograma com distribuição de tweets ao longo do tempo
ggplot(data = tweets, aes(x = timestamp)) +
  geom_histogram(aes(fill = ..count..)) +
  theme(legend.position = "none") +
  xlab("Time") + ylab("Número de tweets") + 
  scale_fill_gradient(low = "midnightblue", high = "aquamarine4")

# Tweets por ano
ggplot(data = tweets, aes(x = year(timestamp))) +
  geom_bar(stat = "count", aes(fill = ..count..)) +
  theme(legend.position = "none") +
  xlab("Time") + ylab("Número de tweets") + 
  scale_fill_gradient(low = "midnightblue", high = "aquamarine4")

# Tweets por dia da semana
ggplot(data = tweets, aes(x = wday(timestamp, label = TRUE))) +
  geom_bar(stat = "count", aes(fill = ..count..)) +
  theme(legend.position = "none") +
  xlab("Dia da Semana") + ylab("Número de tweets") + 
  scale_fill_gradient(low = "midnightblue", high = "aquamarine4")


# Testes Estatísticos

# Um teste apropriado nesse caso é um teste de qui-quadrado de amostra única.
# Com esse teste poderemos ver se a distribuição que temos é consistente com uma determinada hipótese, 
# dentro de erro de amostragem aleatória. 
# Por exemplo, poderíamos ter obtido esta distribuição de tweets apenas por acaso, ou realmente temos menos tweets 
# nos fins de semana? Este tipo de teste pode ser feito com qualquer tipo de frequência esperada, 
# mas primeiro vamos comparar contra a hipótese de frequências esperadas iguais, ou seja, a hipótese de que a DSA "tweeta" 
# na mesma taxa em todos os dias e que essa distribuição de tweets seja apenas por acaso e amostragem aleatória.
chisq.test(table(wday(tweets$timestamp, label = TRUE)))

# O teste do qui-quadrado indica que a distribuição de meus tweets é altamente improvável de ser uniforme.
# Podemos rejeitar a hipótese nula (a hipótese de que a DSA "tweeta" na mesma taxa em todos os dias), 
# com um alto grau de confiança. 

# A distribuição nos tweets pode ser explicada apenas como uma diferença entre o comportamento 
# dos dias úteis e dos fins de semana? 
# Parece que de segunda a quinta-feira são mais altos do que sexta-feira a domingo.
myTable <- table(wday(tweets$timestamp, label = TRUE))
mean(myTable[c(2:5)])/mean(myTable[c(1,6,7)])

# Os valores para segunda-feira a quinta-feira são 1.370274 mais elevados do que os outros dias, em média, perto de 5/4. 
# Vamos ver se o teste chi-quadrado diz que o meu padrão de tweets é consistente com "tweetar" 1.37 vezes mais vezes  
# entre segunda-feira e quinta-feira do que entre sexta-feira até domingo.
chisq.test(table(wday(tweets$timestamp, label = TRUE)), p = c(4, 5, 5, 5, 5, 4, 4)/32)

# O valor-p aqui é ainda muito baixo, assim nós podemos rejeitar esta hipótese simples. 


# Visualizando os Resultados

# Padrão mensal de tweets 
ggplot(data = tweets, aes(x = month(timestamp, label = TRUE))) +
  geom_bar(aes(fill = ..count..)) +
  theme(legend.position = "none") +
  xlab("Mês") + ylab("Número de tweets") + 
  scale_fill_gradient(low = "midnightblue", high = "aquamarine4")

# Este é um padrão anual bastante interessante. A diminuição em janeiro e fevereiro faz sentido porque esta é geralmente 
# uma época de férias. 

# Distribuição de tweets por hora
tweets$timeonly <- as.numeric(tweets$timestamp - trunc(tweets$timestamp, "days"))
tweets[(minute(tweets$timestamp) == 0 & second(tweets$timestamp) == 0),11] <- NA
mean(is.na(tweets$timeonly))

class(tweets$timeonly) <- "POSIXct"
ggplot(data = tweets, aes(x = timeonly)) +
  geom_histogram(aes(fill = ..count..)) +
  theme(legend.position = "none") +
  xlab("Time") + ylab("Número de tweets") + 
  scale_x_datetime(breaks = date_breaks("3 hours"), 
                   labels = date_format("%H:00")) +
  scale_fill_gradient(low = "midnightblue", high = "aquamarine4")

# Tweets no fim do dia
latenighttweets <- tweets[(hour(tweets$timestamp) < 6),]
ggplot(data = latenighttweets, aes(x = timestamp)) +
  geom_histogram(aes(fill = ..count..)) +
  theme(legend.position = "none") +
  xlab("Time") + ylab("Número de tweets") + ggtitle("Tweets Noturnos") +
  scale_fill_gradient(low = "midnightblue", high = "aquamarine4")

# Tweets com ou sem hashtags
ggplot(tweets, aes(factor(grepl("#", tweets$text)))) +
  geom_bar(fill = "midnightblue") + 
  theme(legend.position="none", axis.title.x = element_blank()) +
  ylab("Número de tweets") + 
  ggtitle("Tweets com Hashtags") +
  scale_x_discrete(labels=c("Sem hashtags", "Tweets com hashtags"))

# Retweets
ggplot(tweets, aes(factor(!is.na(retweeted_status_id)))) +
  geom_bar(fill = "midnightblue") + 
  theme(legend.position="none", axis.title.x = element_blank()) +
  ylab("Número de tweets") + 
  ggtitle("Retweeted Tweets") +
  scale_x_discrete(labels=c("Sem retweet", "Com Retweeted"))

# Tweets respondidos
ggplot(tweets, aes(factor(!is.na(in_reply_to_status_id)))) +
  geom_bar(fill = "midnightblue") + 
  theme(legend.position="none", axis.title.x = element_blank()) +
  ylab("Número de tweets") + 
  ggtitle("Tweets Respondidos") +
  scale_x_discrete(labels=c("Sem resposta", "Com resposta"))

# Unindo todas as informaçções em um único gráfico
tweets$type <- "tweet"
tweets[(!is.na(tweets$retweeted_status_id)),12] <- "RT"
tweets[(!is.na(tweets$in_reply_to_status_id)),12] <- "reply"
tweets$type <- as.factor(tweets$type)
tweets$type = factor(tweets$type,levels(tweets$type)[c(3,1,2)])

ggplot(data = tweets, aes(x = timestamp, fill = type)) +
  geom_histogram() +
  xlab("Time") + ylab("Número de tweets") +
  scale_fill_manual(values = c("midnightblue", "deepskyblue4", "aquamarine3"))






































