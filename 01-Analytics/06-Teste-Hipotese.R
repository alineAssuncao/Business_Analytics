# Testes de Hipóteses

# Obs: Caso tenha problemas com a acentuação, consulte este link:
# https://support.rstudio.com/hc/en-us/articles/200532197-Character-Encoding

# Configurando o diretório de trabalho
# Coloque entre aspas o diretório de trabalho que você está usando no seu computador
# Não use diretórios com espaço no nome
setwd("Z:/Dropbox/DSA/Business-Analytics/R/Cap02")
getwd()


# O teste de hipóteses é uma ferramenta estatística útil que pode ser usada para tirar uma conclusão 
# sobre a população a partir de uma amostra. Aqui vai um pequeno resumo bastante útil:

# Sumarizar vs. Inferir
# Média amostral (SD)
# Média das amostras (SE)

# Diferença entre SD e SE. SD é descritivo, SE é inferencial. 

# Quando não sabemos o desvio das amostragens, utilizamos o desvio de uma amostra para calcular o SE 
# Quanto menor o erro padrão (SE), mais confiante estamos na nossa amostra particular.

# Pacotes
install.packages("dplyr")
install.packages('nycflights13')
library('ggplot2')
library('dplyr')
library('nycflights13')
head(flights)

# Considerando o data set como a população
# Considere que há 17000 observações de cada e que trata-se da população.
pop = na.omit(flights) %>% 
  filter(carrier == 'UA' | carrier == 'DL', arr_delay >= 0) %>%
  select(carrier,arr_delay) %>%
  group_by(carrier) %>%
  sample_n(17000) %>%
  ungroup()

# Gerando 2 amostras de 1000 observações cada uma
amostra1 = na.omit(pop) %>% 
  select(carrier,arr_delay) %>%
  filter(carrier == 'DL') %>%
  mutate(sample_id = '1') %>%
  sample_n(1000)

amostra2 = na.omit(pop) %>% 
  select(carrier,arr_delay) %>%
  filter(carrier == 'DL') %>%
  mutate(sample_id = '2') %>%
  sample_n(1000)

samples = rbind(amostra1,amostra2)

# Calculando o intervalo de confiança (95%) da amostra1
se = sd(amostra1$arr_delay) / sqrt(nrow(amostra1))
lower = mean(amostra1$arr_delay) - 1.96 * se
upper = mean(amostra1$arr_delay) + 1.96 * se
ic_1 = c(lower,upper)
mean(amostra1$arr_delay)
ic_1

# Calculando o intervalo de confiança (95%) da amostra2
se = sd(amostra2$arr_delay) / sqrt(nrow(amostra2))
lower = mean(amostra2$arr_delay) - 1.96 * se
upper = mean(amostra2$arr_delay) + 1.96 * se
ic_2 = c(lower,upper)
mean(amostra2$arr_delay)
ic_2

# Visualizando os intervalos de confiança
toPlot = summarise(group_by(samples, sample_id), mean = mean(arr_delay))
toPlot = mutate(toPlot, lower = ifelse(toPlot$sample_id == 1,ic_1[1],ic_2[1]))
toPlot = mutate(toPlot, upper = ifelse(toPlot$sample_id == 1,ic_1[2],ic_2[2]))
ggplot(toPlot, aes(x = sample_id, y=mean, colour = sample_id )) + 
  geom_point() +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.1)

# Podemos dizer que muito provavelmente, as amostras vieram da mesma população.


######-------- Teste de Hipóteses ---------

# Hipótese nula: não haver diferenças. Algo que queremos rejeitar com nosso experimento.

# Hipótese alternativa: uma teoria que você deseja reforçar. Nunca provar! 
# Estamos falando de probabilidade da hipótese alternativa ser verdadeira.

# Teste estatístico: (efeito (variancia explicada pelo modelo) / erro (não explicada) ).
# Nos permite utilizar métodos estatísticos para tomar decisões. Tipicamente, comparando duas ou mais 
# soluções/alternativas em um mesmo contexto
# Estabelece um nível de probabilidade (p-value) através do qual consideramos os resultados da nossa amostra 
# confiáveis o suficiente para rejeitar a hipótese nula.

# Note que tem uma semântica muito parecida com teste de sistemas. 
# Falha ao rejeitar a hipótese nula não significa dizer que ela é verdadeira. 
# Somente significa que o experimento não encontrou evidencias para rejeitá-la.

# Ho e H1 devem ser mutuamente exclusivas.

# Rejeitar a hipótese nula – Encontrar significância ou resultado significante
# Não rejeitar a hipótese nula (fail to reject) – Não prova que ela é verdadeira, indica que não encontramos no estudo específico evidências suficientes para rejeitá-la
# Um número no intervalo [0,1] chamado p-valor (p-value) – Um nível de probabilidade


# Comparando alternativas: DL atrasa mais que UA?

# Ho = Não há diferença significativa entre os atrasos da DL e UA (diff da média de atrasos = 0).
# H1 = Delta atrasa mais (diff das médias > 0).

dl <- sample_n(filter(pop, carrier == "DL", arr_delay > 0), 1000)
ua <- sample_n(filter(pop, carrier == "UA", arr_delay > 0), 1000)

diff_amostras = mean(dl$arr_delay) - mean(ua$arr_delay)
diff_amostras

# Single tail (95%)
tabela = 1.65

# Estatísticas DL
mean_dl = mean(dl$arr_delay)
sd_dl = sd(dl$arr_delay)

# Estatísticas UA
mean_ua = mean(ua$arr_delay)
sd_ua = sd(ua$arr_delay)

# Calculando o valor crítico
se_diff = sqrt( ((sd_dl*sd_dl) / 1000) + ( (sd_ua*sd_ua) / 1000 ) )
critical = 1.65 * se_diff

# Comparando
critical

# Diferença
diff_amostras

# Falhamos em rejeitar a hipótese nula, pois p-valor é maior que o limite.
# Significa dizer que há uma probabilidade alta de não haver diferença significativa entre os atrasos.
# Para os nossos dados, não há evidência estatística de que a DL atrase mais que a UA

# Com ICS
se = sd(dl$arr_delay) / sqrt(nrow(dl))
mean(dl$arr_delay)

lower = mean(dl$arr_delay) - 1.96 * se
upper = mean(dl$arr_delay) + 1.96 * se
ic_dl = c(lower,upper)
ic_dl

se = sd(ua$arr_delay) / sqrt(nrow(ua))
mean(ua$arr_delay)

lower = mean(ua$arr_delay) - 1.96 * se
upper = mean(ua$arr_delay) + 1.96 * se
ic_ua = c(lower,upper)
ic_ua

# Teste t
t.test(dl$arr_delay,ua$arr_delay, alternative="greater")


# P-valor
# Propabiblidade de que a estatística do teste assuma um valor extremo em relação ao valor observado quando H0 é verdadeira.

# Lembre-se disso:
# Baixo valor p: forte evidência empírica contra h0
# Alto valor p: pouca ou nenhuma evidência empírica contra h0

