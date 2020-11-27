# Análise de Regressão

# Obs: Caso tenha problemas com a acentuação, consulte este link:
# https://support.rstudio.com/hc/en-us/articles/200532197-Character-Encoding

# Configurando o diretório de trabalho
# Coloque entre aspas o diretório de trabalho que você está usando no seu computador
# Não use diretórios com espaço no nome
setwd("/home/aline/Projetos/2")
getwd()

# Explorando Testes "Motor Trend Car Road"

# Neste projeto, trabalhamos para a Motor Trend (uma revista sobre a indústria automobilística) e 
# vamos analisar o conjunto de dados mtcars. Observando um conjunto de dados de uma coleção de 
# carros (que contém 32 observações), eles estão interessados em explorar a relação entre um 
# conjunto de variáveis e a autonomia de combustível em milhas por galão (mpg), que é a nossa variável target. 
# Estão particularmente interessados nestas duas questões:

# Qual tipo de transmissão consome menos combustível, automática ou manual?
# Quão diferente é o mpg entre as transmissões 'Automática' e 'Manual'?


#####--------------------- Pré-processamento e Transformações de Dados -----------------------------
# Carga de dados
data(mtcars)
head(mtcars)

# mpg	- Miles/gallon
# cyl	- Number of cylinders
# disp	- Displacement (cu.in.)
# hp	- Gross horsepower
# drat	- Rear axle ratio
# wt	- Weight (1000 lbs)
# qsec	- 1/4 mile time
# vs	- V/S
# am	- Transmission (0 = automatic, 1 = manual)
# gear	- Number of forward gears
# carb	- Number of carburetors


#####--------------------- Análise Exploratória -----------------------------
# Exploramos várias relações entre as variáveis de interesse e o resultado (target). Inicialmente, traçamos as 
# relações entre todas as variáveis do conjunto de dados conforme gráfico abaixo. 
# A partir da matriz de correlação notamos que variáveis como cyl, disp, hp, drat, wt, vs e am parecem ter
# alguma forte correlação com mpg. Usaremos modelos lineares para quantificar isso na próxima seção.

# Além disso, traçamos um boxplot da variável mpg quando am é 'Automático' ou 'Manual' 
# Este gráfico mostra que o mpg aumenta quando a transmissão é 'Manual'.
install.packages("corrplot")
library(corrplot)
par(mfrow=c(1,1))

# Corrplot
m_cor <- cor(mtcars)
corrplot(m_cor, method = "circle")

# Pair Plot
pairs(mtcars, panel=panel.smooth, main="Pair Graph of Motor Trend Car Road Tests")

# Boxplot
boxplot(mtcars$mpg ~ mtcars$am)


#####--------------------- Extração e transformação -----------------------------
# Inicialmennte carregamos o conjunto de dados, realizamos as transformações de dados necessárias, 
# fatorizamos algumas variáveis e observando os dados:
mtcars$cyl <- factor(mtcars$cyl)
mtcars$vs <- factor(mtcars$vs)
mtcars$gear <- factor(mtcars$gear)
mtcars$carb <- factor(mtcars$carb)
mtcars$am <- factor(mtcars$am,labels=c('Automatic','Manual'))

# Resumo
str(mtcars)


#####--------------------- Análise de Regressão -----------------------------
# Nesta seção, vamos construir modelos de regressão linear com base nas diferentes variáveis de 
# interesse e tentar descobrir o melhor modelo de ajuste. Vamos compará-lo com o modelo base que 
# temos usando ANOVA. Após a seleção do modelo, realizaremos uma análise dos resíduos.


#####--------------------- Construção do Modelo -----------------------------
# Com base na matriz de correlação, existem diversas variáveis que parecem ter alta correlação com mpg. 
# Iremos construir um modelo inicial com todas as variáveis como preditores e usaremos o método de seleção stepwise
# para selecionar os preditores mais sgnificantes para a versão final do modelo. Isto é obtido pelo método stepwise, 
# que executa a função lm() várias vezes para construir modelos de regressão múltipla e selecionar as melhores 
# variáveis a partir deles, usando tanto a seleção direta (forward selection) e 
# métodos de eliminação (backward elimination) através do algoritmo AIC.
?lm
mod_init <- lm(mpg ~ ., data = mtcars)
?step
mod_best <- step(mod_init, direction = "both")

summary(mod_best)

# Podemos ver que o valor de R2 ajustado é igual a 0,84, que é o máximo obtido considerando 
# todas as combinações de variáveis. Portanto, podemos concluir que mais de 84% da variabilidade 
# é explicada por este modelo.

# Agora, usando ANOVA, vamos comparar o modelo base com apenas a variável "am" como a variável preditora e o 
# melhor modelo obtido acima:
mod_base <- lm(mpg ~ am, data = mtcars)
anova(mod_best, mod_base)

# Considerando este resultado, o valor p obtido é altamente significativo e rejeitamos a hipótese 
# nula de que as variáveis cyl, hp e wt não contribuem para a precisão do modelo.


#####--------------------- Análise dos Residuais e Diagnósticos -----------------------------
# Agora exploramos os plots dos resíduos do nosso modelo de regressão e também calculamos alguns 
# dos diagnósticos de regressão de nosso modelo para descobrir outliers no conjunto de dados:
par(mfrow=c(2,2))
plot(mod_best, which=1)
plot(mod_best, which=2)
plot(mod_best, which=3)
plot(mod_best, which=5)

# A partir destes plots podemos concluir o seguinte:

# O gráfico Residuals vs Fitted mostra pontos aleatórios no gráfico que verifica a condição de independência.
# Na plot Normal Q-Q os pontos caem principalmente sobre a linha indicando que os resíduos são normalmente distribuídos.
# No gráfico Scale-Location os pontos estão em um padrão constante, indicando variância constante.
# Finalmente, o gráfico Residuals vs Leverage mostra alguns pontos de interesse (outliers ou pontos de alavancagem) 
# estão no canto superior direito.

# Agora vamos calcular alguns diagnósticos de regressão do nosso modelo para descobrir quais são os outliers. 
# Calculamos os três primeiros pontos em cada caso de medidas de influência.
lev <- hatvalues(mod_best)
tail(sort(lev),3)

inf <- dfbetas(mod_best)
tail(sort(inf[,3]),3)

# Olhando para este resultado vemos que eles são os mesmos carros mostrados nos Plots dos resíduos.

# Inferência Estatística

# Finalmente, realizaremos um teste t assumindo que os dados de transmissão têm uma distribuição normal 
# e veremos que as transmissões manuais e automáticas são significativamente diferentes:
t.test(mpg ~ am, data = mtcars)


#####--------------------- Conclusões -----------------------------
# A partir do resumo (mod_best) podemos concluir o seguinte:
coefficients(mod_best)
confint(mod_best)
fitted(mod_best)
influence(mod_best)

par(mfrow=c(2,2))
termplot(mod_best)

# Milhas por galão (mpg) irá aumentar em 1.81 em carros com transmissão 'Manual' 
# em comparação com carros com transmissão 'Automatic' (ajustado por hp, cyl e wt). 
# Conclusão para a Motor Trend Magazine é: 'Transmissão manual' é melhor para mpg.

# Milhas por galão (mpg) irá diminuir em 2,5 por cada 1000 lb de aumento em peso (ajustado por hp, cyl e am).
# Conclusão: Milhas por galão (mpg) diminui com aumento de wt.

# Milhas por galão (mpg) irá diminuir em um fator de 3 e 2,2 se o número de cilindros cyl 
# aumentar de 4 para 6 e 8, respectivamente (ajustado por hp, wt e am).






