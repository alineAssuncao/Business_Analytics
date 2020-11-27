# Facebook Analytics
# https://github.com/pablobarbera/Rfacebook

# Obs: Caso tenha problemas com a acentuação, consulte este link:
# https://support.rstudio.com/hc/en-us/articles/200532197-Character-Encoding

# Configurando o diretório de trabalho
# Coloque entre aspas o diretório de trabalho que você está usando no seu computador
# Não use diretórios com espaço no nome
setwd("Z:/Dropbox/DSA/Business-Analytics/R/Cap08/Facebook")
getwd()

# Instalando pacote RFacebook a partir do Github
library(devtools)
install_github("pablobarbera/Rfacebook", subdir = "Rfacebook", force = TRUE)

# Carregando o pacote
library(Rfacebook)

# Usando token com duração de 2 horas
token <- "xxx"

# Configurando autenticação
# https://developers.facebook.com/apps/
require("Rfacebook")

# Autenticando por 2 meses
fb_oauth <- fbOAuth(app_id = "xxxx", app_secret = "xxxx", extended_permissions = TRUE)

# Salvando a configuração
save(fb_oauth, file = "fb_oauth")
load("fb_oauth")


## Dados de Páginas no Facebook

### TROCANDO NOME DA PAGINA NVIDIA
# Obtém dados de uma página
?getPage
page <- getPage("NVIDIAGeForce.BR", token, n = 400)
View(page)
class(page)

# Busca por páginas
?searchPages
pages <- searchPages(string = "Data Science", token = token, n = 200)
View(pages)

# Post com maior número de likes
page[which.max(page$likes_count), ]

# Filtrando pela data mais recente
pageRecent <- page[which(page$created_time > "2017-04-02"), ]

# Ordenando pelo número de likes
top <- pageRecent[order(- pageRecent$likes),]
head(top, n = 2)

# Verificando trend
post1 <- getPost("130196237030986_1290400904343841", token, n = 1000, likes = FALSE, comments = FALSE)
post2 <- getPost("130196237030986_1317789011605030", token, n = 1000, likes = FALSE, comments = FALSE)
View(post2)


# Usando Linguagem SQL e Minerando dados do Facebook
post_id <- head('130196237030986_1290400904343841', n = 1)  

# Busca likes e comentários
post <- getPost(post_id, token, n = 1000, likes = TRUE, comments = TRUE)
head(post$comments, n = 2)

# Gravando os comentários
comments <- post$comments
class(comments)
View(comments)

# Usando SQL
install.packages("sqldf")
library(sqldf)

# Usuários mais influenciadores
infusers <- sqldf("select from_name, sum(likes_count) as totlikes from comments group by from_name")
head(infusers)

# Buscando os "top" influenciadores
infusers$totlikes <- as.numeric(infusers$totlikes)
top <- infusers[order(- infusers$totlikes),]
head(top, n = 10)

# Buscando comentários de vários posts simultaneamente
post_id <- head(page$id, n = 100)
head(post_id, n = 10)
post_id <- as.matrix(post_id)
allcomments <- ""

# Percorrendo todos os posts e coletando comentários de todos eles
for (i in 1:nrow(post_id))
{
  post <- getPost(post_id[i,], token, n = 1000, likes = TRUE, comments = TRUE)
  comments <- post$comments
  allcomments <- rbind(allcomments, comments)
}

# Usuários mais influentes
infusers <- sqldf("select from_name, sum(likes_count) as totlikes from allcomments group by from_name")
infusers$totlikes <- as.numeric(infusers$totlikes)
top <- infusers[order(- infusers$totlikes),]
head(top, n = 20)
View(allcomments)


# Performance da página

# Converte o formato de data do Facebook para o formato do R
format.facebook.date <- function(datestring) {
  date <- as.POSIXct(datestring, format = "%Y-%m-%dT%H:%M:%S+0000", tz = "GMT")
}

# Agregando a métrica de counts por mês
aggregate.metric <- function(metric) {
  m <- aggregate(page[[paste0(metric, "_count")]], list(month = page$month), mean)
  m$month <- as.Date(paste0(m$month, "-15"))
  m$metric <- metric
  return(m)
}

# Obtendo os posts da página da Nvidia
page <- getPage("NVIDIAGeForce.BR", token, n = 500)

# Aplicando a agregação aos dados coletados
page$datetime <- format.facebook.date(page$created_time)
page$month <- format(page$datetime, "%Y-%m")
df.list <- lapply(c("likes", "comments", "shares"), aggregate.metric)
df <- do.call(rbind, df.list)

# Plot
library(ggplot2)
library(scales)

ggplot(df, aes(x = month, y = x, group = metric)) + 
  geom_line(aes(color = metric)) + 
  scale_y_log10("Média de Counts por Mês", breaks = c(10, 100, 1000, 10000, 50000)) + 
  theme_bw() + 
  theme(axis.title.x = element_blank()) + 
  ggtitle("Performance da Página no Facebook")

# Salvando a imagem 
ggsave(file="chart.png", dpi = 500)


