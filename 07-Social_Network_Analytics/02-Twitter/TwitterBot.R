# Social Network Analytics - Twitter Bot
# Coletando dados da web e twittando em tempo real

# Obs: Caso tenha problemas com a acentuação, consulte este link:
# https://support.rstudio.com/hc/en-us/articles/200532197-Character-Encoding

# Configurando o diretório de trabalho
# Coloque entre aspas o diretório de trabalho que você está usando no seu computador
# Não use diretórios com espaço no nome
setwd("Z:/Dropbox/DSA/Business-Analytics/R/Cap08/Twitter")
getwd()


# Instalando pacotes
# install.packages("rvest")
# install.packages("stringr")
# install.packages("dplyr")
# install.packages("twitteR")

# Carregando os pacotes
library(rvest)
library(stringr)
library(dplyr)
library(twitteR)

# Web Scraping
url <- "https://cran.r-project.org/web/packages"

# Lendo a url
page <- read_html(url)

# Obtendo o num de pacotes
n_packages <- page %>%
  html_text() %>% 
  str_extract("[[:digit:]]* available packages") %>% 
  str_extract("[[:digit:]]*") %>% 
  as.numeric()

print(n_packages)

# Num de pacotes lidos na ultima leitura
n_packages_last_time <- read.table(file = "n_packages.csv",stringsAsFactors = F, sep = ";")
n_packages_last_time <- n_packages_last_time$V2[nrow(n_packages_last_time)]

# Envia tweet somente se o num de pacotes mudou desde a ultima leitura
if(n_packages > n_packages_last_time) {

  # Definindo as chaves de acesso
  api_key <- "xxx"
  api_secret <- "xxx"
  access_token <- "xxx"
  access_token_secret <- "xxx"
  
  # Autenticando no Twitter
  setup_twitter_oauth(api_key, api_secret, access_token, access_token_secret)
  
  # Time
  time <- Sys.time()
  
  # Cria o tweet
  tweet_text <- paste0("Olá a todos, são ", time, " e neste momento existem ", n_packages, " pacotes R no CRAN. Aqui é o TweetBot DSA em ação!")
  
  # Envia o tweet
  tweet(tweet_text)
  
  # Gravando no arquivo
  n_packages_df <- data.frame(time = Sys.time(), n = n_packages)
  write.table(n_packages_df, file = "n_packages.csv", row.names = FALSE,col.names = FALSE, append = TRUE, sep = ";")
  
}
