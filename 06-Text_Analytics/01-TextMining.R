# Text Analytics em R

# Download do http://www.foolabs.com/xpdf/download.html

# Diretório de trabalho
setwd("/home/aline/Projetos/7")
getwd()

# Instalar os Pacotes
install.packages("tm")                           # Framework de text mining
install.packages("qdap")                         # Análise quantitativa do discurso de trasncripts
install.packages("qdapDictionaries")             # Dicionários
install.packages("dplyr")                        # Manipulação de Dados
install.packages("RColorBrewer")                 # Paleta de cores
install.packages("ggplot2")                      # Gráficos
install.packages("scales")                       # Permite incluir vírgulas em números, criando milhares 

# http://bioconductor.org/
source("http://bioconductor.org/biocLite.R")     # Biocondutor
biocLite("Rgraphviz")                            # Plots de correlação entre palavras

# Carregados os pacotes
library(tm) 
library(qdap) 
library(qdapDictionaries)
library(dplyr) 
library(RColorBrewer) 
library(ggplot2) 
library(scales) 
library(Rgraphviz) 

# Visualizando as fontes de dados
getSources()

# Leitores
getReaders()

# Pasta com os arquivos pdf
dest <- "/home/aline/Projetos/7/corpus/pdf"

# Vetor com o nome dos arquivos pdf
myfiles <- list.files(path = dest, pattern = "pdf",  full.names = TRUE)

# Converte pdf para txt
lapply(myfiles, function(i) system(paste('"/Users/dmpm/xpdfbin-mac-3.04/bin64/pdftotext"', paste0('"', i, '"')), wait = FALSE) )
#lapply(myfiles, function(i) system(paste('"C:/xpdf-tools/bin64/pdftotext"', paste0('"', i, '"')), wait = FALSE) )

# Pastas de origem e destino
current.folder <- "/home/aline/Projetos/7/corpus/pdf"
new.folder <- "/home/aline/Projetos/7/corpus/txt"

# Buscando os arquivos txt
list.of.files <- list.files(current.folder, "*.txt$", full.names = T)
list.of.files


### CRIANDO A PASTA ANTES DE COPIAR OS ARQUIVOS. 
dir.create(new.folder, recursive = TRUE)

# Copiando os arquivos para a nova pasta
file.copy(from = list.of.files, to = new.folder)

# Definindo o diretório do Corpus
cname <- file.path(".", "corpus", "txt")
length(dir(cname))
dir(cname)

# Definindo a fonte de dados e explorando o corpus
docs <- Corpus(DirSource(cname))

class(docs)
class(docs[[1]])
summary(docs)
inspect(docs[16])

#tm_map(title_corpus, enc2utf8)
a <- sapply(docs, function(x){
  enc2utf8(x)
})


# docs <- Corpus(DirSource(cname), readerControl=list(reader=readPDF))
# docs <- Corpus(DirSource(cname), readerControl=list(reader=readDOC))

# Preparando o Corpus
getTransformations()
toSpace <- content_transformer(function(x, pattern) gsub(pattern, " ", x))
docs <- tm_map(docs, toSpace, "/")
docs <- tm_map(docs, toSpace, "@")
docs <- tm_map(docs, toSpace, "\\|")
docs <- tm_map(docs, toSpace, "/|@|\\|")
docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, removePunctuation)


# Removendo as stopwords
length(stopwords("english"))
stopwords("english")
docs <- tm_map(docs, removeWords, stopwords("english"))
docs <- tm_map(docs, removeWords, c("department", "email", "aaa"))
docs <- tm_map(docs, stripWhitespace)

# Convertendo texto para siglas
toString <- content_transformer(function(x, from, to) gsub(from, to, x))
docs <- tm_map(docs, toString, "harbin institute technology", "HIT")
docs <- tm_map(docs, toString, "shenzhen institutes advanced technology", "SIAT")
docs <- tm_map(docs, toString, "chinese academy sciences", "CAS")

# Stemming
docs <- tm_map(docs, stemDocument)

### CONVERTENDO O ENCONDING DO DOC. PROBLEMAS NO WINDOWS
docs <- tm_map(docs, enc2native)

# Uma matriz de termo de documento é simplesmente uma matriz com documentos como as linhas e termos como as colunas
# E uma contagem da frequência de palavras como as células da matriz. 
# Usamos DocumentTermMatrix() para criar a matriz:
dtm <- DocumentTermMatrix(docs)

# A matriz do termo do documento é de fato bastante esparsa (isto é, na maior parte vazia) e assim é realmente
# armazenada em uma representação muito mais compacta internamente. 
# Podemos ainda obter a contagem de linhas e colunas 
class(dtm)
inspect(dtm[1:5, 1000:1005])
dim(dtm)

# A transposição é criada usando TermDocumentMatrix():
tdm <- TermDocumentMatrix(docs)
tdm

# Podemos obter as frequências dos termos como um vetor convertendo a matriz do termo do documento em uma
# matriz e somando as contagens das colunas:
freq <- colSums(as.matrix(dtm))
length(freq)
ord <- order(freq)
freq[head(ord)]
freq[tail(ord)]

# Distribuição das Frequências dos Termos
head(table(freq), 15)
tail(table(freq), 15)

# Convertendo a Matriz para csv e gravando
m <- as.matrix(dtm)
dim(m)
write.csv(m, file="dtm.csv")

# Removendo termos esparsos
# Muitas vezes não estamos interessados em termos infrequentes em nossos documentos. 
# Tais termos "esparsos" podem ser removidos da matriz do termo do documento muito facilmente usando 
# removeSparseTerms ():
dim(dtm)
dtms <- removeSparseTerms(dtm, 0.1)
dim(dtms)
inspect(dtms)

# Podemos ver o efeito observando os termos que deixamos:
freq <- colSums(as.matrix(dtms))
freq

# Identificando termos frequentes e associações
findFreqTerms(dtm, lowfreq = 1000)
findFreqTerms(dtm, lowfreq = 100)

# Buscando associações com palavras e especificando o limite de correlação
# Se duas palavras aparecerem sempre juntas então a correlação seria 1.0 e se elas nunca aparecerem
# a correlação seria de 0,0. Assim, a correlação é uma medida de quão as palavras estão juntas no corpus.
findAssocs(dtm, "data", corlimit = 0.6)

# Plot de correlação
plot(dtm, terms = findFreqTerms(dtm, lowfreq = 100)[1:50], corThreshold = 0.5)

# Plot da frequência das palavras
freq <- sort(colSums(as.matrix(dtm)), decreasing = TRUE)
head(freq, 14)

# Correlações
wf <- data.frame(word=names(freq), freq=freq)
head(wf)

# Plot com ggplot2
subset(wf, freq>500) %>%
  ggplot(aes(word, freq)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Visualização de Dados
install.packages("wordcloud")
library(wordcloud)

set.seed(142)
wordcloud(names(freq), freq, max.words = 100)

set.seed(142)
wordcloud(names(freq), freq, min.freq = 100, colors = brewer.pal(6, "Dark2"))

set.seed(142)
wordcloud(names(freq), freq, min.freq = 100, scale=c(5, .1), colors = brewer.pal(6, "Dark2"))

set.seed(142)
dark2 <- brewer.pal(6, "Dark2")
wordcloud(names(freq), freq, min.freq = 100, rot.per = 0.2, colors = dark2)

# Ajustando os dados e calculando estatísticas
words <- dtm %>%
  as.matrix %>%
  colnames %>%
  (function(x) x[nchar(x) < 20])

length(words)
head(words, 15)
summary(nchar(words))
table(nchar(words))
dist_tab(nchar(words))

data.frame(nletters=nchar(words)) %>%
  ggplot(aes(x=nletters)) +
  geom_histogram(binwidth=1) +
  geom_vline(xintercept=mean(nchar(words)),
             colour="green", size=1, alpha=.5) +
  labs(x="Número de Letras", y="Número de Palavras")

install.packages("stringr")
library(stringr)

words %>%
  str_split("") %>%
  sapply(function(x) x[-1]) %>%
  unlist %>%
  dist_tab %>%
  mutate(Letter=factor(toupper(interval),
                       levels=toupper(interval[order(freq)]))) %>%
  ggplot(aes(Letter, weight=percent)) +
  geom_bar() +
  coord_flip() +
  labs(y="Proporção") +
  scale_y_continuous(breaks=seq(0, 12, 2),
                     label=function(x) paste0(x, "%"),expand=c(0,0), limits=c(0,12))


words %>%
  lapply(function(x) sapply(letters, gregexpr, x, fixed=TRUE)) %>%
  unlist %>%
  (function(x) x[x!=-1]) %>%
  (function(x) setNames(x, gsub("\\d", "", names(x)))) %>%
  (function(x) apply(table(data.frame(letter=toupper(names(x)),
                                      position=unname(x))),
                     1, function(y) y/length(x))) %>%
  qheat(high="green", low="yellow", by.column=NULL,
        values=TRUE, digits=3, plot=FALSE) +
  labs(y="Letra", x="Posição") +
  theme(axis.text.x=element_text(angle=0)) +
  guides(fill=guide_legend(title="Proporção"))





