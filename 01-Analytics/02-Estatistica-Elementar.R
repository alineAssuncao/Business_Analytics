# Estatística Elementar em R

# Obs: Caso tenha problemas com a acentuação, consulte este link:
# https://support.rstudio.com/hc/en-us/articles/200532197-Character-Encoding

# Configurando o diretório de trabalho
# Coloque entre aspas o diretório de trabalho que você está usando no seu computador
# Não use diretórios com espaço no nome
setwd("/home/aline/Projetos/2")
getwd()

# Pacote para as funções skewness e kurtosis
install.packages("e1071")
library(e1071) 

# Datasets
?faithful
duration = faithful$eruptions  
waiting = faithful$waiting 

# A média de uma variável de observação é uma medida numérica da localização central dos valores dos dados. 
# É a soma de seus valores de dados divididos pela contagem de dados.
mean(duration)  

# A mediana de uma variável é o valor no meio quando os dados são classificados em ordem crescente. 
# É uma medida ordinal da localização central dos valores de dados.
median(duration) 

# Existem vários quartis de uma variável. O primeiro quartil, ou quartil inferior, é o valor 
# que corta os primeiros 25% dos dados quando é classificado em ordem crescente. 
# O segundo quartil, ou mediana, é o valor que corta os primeiros 50%. 
# O terceiro quartil, ou quartil superior, é o valor que corta os primeiros 75%.
quantile(duration)     
quantile(duration, c(.32, .57, .98)) 

# O intervalo de uma variável é a diferença de seus maiores e menores valores de dados. 
# É uma medida de quão distante os dados se espalham.
range(duration)

# A variação interquartil de uma variável é a diferença de seus quartis superior e inferior. 
# É uma medida de quão distante a parte média dos dados se espalha.
IQR(duration)

# A variância é uma medida numérica de como os valores dos dados estão dispersos em torno da média.
var(duration)

# O desvio padrão de uma variável é a raiz quadrada de sua variância.
sd(duration) 

# A covariância de duas variáveis x e y em um conjunto de dados mede como as duas variáveis estão linearmente relacionadas. 
# Uma covariância positiva indicaria uma relação linear positiva entre as variáveis, e uma covariância 
# negativa indicaria o contrário.
cov(duration, waiting)

# O coeficiente de correlação de duas variáveis em um conjunto de dados é igual a sua covariância dividida 
# pelo produto de seus desvios-padrão individuais. É uma medida normalizada de como os dois estão 
# linearmente relacionados. Os valores vão de -1 a +1. Valores próximos de zero indicadm que não há correlação.
# Valor de -1 indica forte correlação negativa e +1 forte correlação positiva.
cor(duration, waiting) 

# O skewness é uma medida de simetria. Como regra, a inclinação negativa indica que a média 
# dos valores dos dados é menor que a mediana e a distribuição dos dados é inclinada para a esquerda. 
# A inclinação positiva indicaria que a média dos valores dos dados é maior do que a mediana e a distribuição 
# dos dados é desviada para a direita.
skewness(duration)

# A curtose descreve a forma da cauda da distribuição de dados. 
# A distribuição normal tem kurtosis igual a zero.
kurtosis(duration)  







