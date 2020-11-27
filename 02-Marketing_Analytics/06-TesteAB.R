#####-------------- Testes AB em R --------------------

##########################################################
###### Leia com atenção cada comentário abaixo, pois #####
###### neles você encontra de forma detalhada como #######
###### realizar e interpretar Testes A/B #################
##########################################################

# Os dados a seguir representam o resultado de Teste A/B em uma home page (landing page) e queremos determinar os resultados.
# Cada linha representa um visitante para a página de destino e 1 representa um visitante convertido em um usuário.
# A coluna A representa os resultados da landing page de controle.
# A coluna B representa os resultados da landing page de teste.
getwd()

dados <- read.csv('data/dados_landing_page.csv', header = T)
View(dados)

# Observando os dados, cada linha representa uma amostra.
# Um visitante é aleatoriamente designado para a página de destino A ou B, e converte ou não converte em um usuário.
# Já que queremos determinar se a taxa de conversão para a página de destino B é maior que a taxa para A, queremos 
# o teste estatístico que permita a comparação das duas taxas de conversão.

# Verificando o número de linhas
nrow(dados)

# Verificando o número de linhas que não contém valores NA's
colSums(!is.na(dados))

# Calculamos aqui o número total de conversões para cada landing page
sum(dados$a, na.rm = TRUE)
sum(dados$b, na.rm = TRUE)


#####-------------- Selecionando o Teste Estatístico --------------------

# T-test é o teste estatístico mais comum para comparar dois grupos e poderia ser usado aqui. 
# Mas o teste mais correto é, no entanto, o teste z para comparar duas PROPORÇÕES.

# NOTA SOBRE o Z-teste:
# O seguinte deve ser verdadeiro para z-tests:
# 1. As duas amostras devem ser independentes.
# 2. Os tamanhos das amostras devem ser suficientemente grandes (> 30) para a distribuição a ser aplicada.
# 3. As amostras devem ser selecionadas aleatoriamente.

# Para este exemplo assumimos que as amostras são independentes, ou seja, 
# um visitante de uma página não tem efeito sobre um visitante de outra página.
# Nossos tamanhos de amostra são grandes o suficiente; Teorema do limite central aplicado.
# Assumimos que o experimento foi randomizado.
# Se nosso tamanho da amostra fosse menor que 30, um teste t seria mais apropriado.


#####-------------- Definindo as Hipóteses --------------------

# No mundo ideal, todas as hipóteses estatísticas seriam testadas em populações inteiras. 
# Contudo, isto é muitas vezes impraticável ou impossível, então normalmente examinamos uma amostra aleatória da População.
# O teste de hipóteses é definido como o processo para o qual você pode testar uma reivindicação sobre um parâmetro da População.
# No exemplo da landing page, o parâmetro da População é a diferença entre as taxas de conversão da landing page B e landing page A.
# Como não temos dados para cada usuário que visitará as duas landing pages
# (o que nos permitiria calcular o verdadeiro parâmetro da população),
# tomamos, portanto, uma amostra, calculamos uma estatística de amostra e, em seguida,
# a probabilidade de que nossa estatística de amostra poderia ter ocorrido,
# assumindo nossa hipótese nula, sobre o parâmetro da população, é verdadeira.
# Em nosso exemplo da landing page, nossa estatística de amostra é calculada a partir dos dados fornecidos.

# Hipóteses Nulas e Alternativas

# A hipótese nula:
# A declaração sobre o parâmetro da população que é assumida como verdadeira,
# geralmente é a hipótese de que a amostra estatística observada foi puramente por acaso.

# A hipótese alternativa:
# A hipótese alternativa contradiz diretamente a hipótese nula e é
# geralmente a hipótese de que a estatística da amostra foi influenciada por alguma causa.

# Nossas hipóteses
# A nossa hipótese nula: a diferença das taxas de conversão entre a landing page B e a landing page A é igual a 0
# A nossa hipótese alternativa: a diferença das taxas de conversão entre a landing page B e a landing page A é maior que 0


#####-------------- Realizando o Z-Test --------------------

# Com nossa estatística de amostra agora devemos normalizá-la.
# Isto é feito convertendo em um z-score. É chamado de z-score porque a distribuição normal também é conhecida como o distribuição Z.
# Podemos usar a seguinte fórmula para calcular o nosso z-score para comparar duas Proporções:

# P1: a taxa de conversão de nossa landing page de teste B
# P2: a taxa de conversão de nossa landing page de controle A
# Y1: o número de sucessos em nossa landing page de teste B
# Y2: o número de sucessos em nossa landing page de controle A
# N1: tamanho total da amostra de A
# N2: tamanho total da amostra de B
# P: proporção combinada

# Para calcular as duas taxas de conversão, podemos simplesmente calcular a proporção de 1s nas duas colunas. 
# O único truque é usar na.omit () para remover o NA na coluna A.

a_obs <- na.omit(dados$a)
b_obs <- na.omit(dados$b)
a_proportion = sum(a_obs)/length(a_obs)
b_proportion = sum(b_obs)/length(b_obs)
a_proportion
b_proportion

# Uma vez que temos nossa estatística de teste (estatística de amostra), então queremos descobrir, 
# quais são as nossas chances de obter esta estatística novamente.
# Calculamos a estatística de teste e a atribuímos à variável test_statistic,
# Verifique o seu cálculo abaixo

test_statistic <- b_proportion - a_proportion
test_statistic

# Calculando a proporção agrupada
# Depois de calcular a estatística de teste, precisamos calcular a estatística agrupada e atribuí-la à variável grouped_proportion.
# Devemos calcular a proporção agrupada porque a nossa hipótese nula considera diferença entre as taxas de conversão sendo igual a 0.

# A proporção agrupada é calculada abaixo

grouped_proportion <- (sum(a_obs) + sum(b_obs)) / (length(a_obs) + length(b_obs))
grouped_proportion


# Calculamos agora a variabilidade da nossa estatística de teste na sua distribuição de amostras; O erro padrão.
# O erro padrão é o denominador da equação para Z.

SError <- sqrt(grouped_proportion*(1-grouped_proportion)*(1/length(a_obs) + 1/(length(b_obs))))
SError

# Calcular o score Z
# Ao dividir nossa estatística de teste por seu erro padrão, criamos o Z-score ou Z em nossa equação. 
# O Z-score nos dá um valor relativo para que possamos determinar quão extrema é a ocorrência da nossa estatística de teste
# com base na distribuição normal.

z_score <- (b_proportion - a_proportion) / SError
z_score

# O score z representa quantos desvios padrão de distância a estatística de teste considera estar, assumindo  a hipótese nula como verdadeira.
# Usamos agora a distribuição normal para determinar qual é a probabilidade de obter um z-score mais extremo do que o que observamos.
# Uma vez que nossa estatística de teste é positiva, queremos olhar para a área sob a cauda direita da distribuição normal

# Para calcular a área sob a curva normal à direita do nosso score z usamos a função pnorm () no R.
# Definimos lower.tail como FALSE para que a função calcule a área sob de forma correta .

p_value <- pnorm(z_score, lower.tail = F)
p_value

# A área sob a curva à direita do z-score é a chance de obter uma estatística de teste tão extrema quanto ou mais extremo do que, 
# o que observamos, dado que a Hipótese nula sendo verdadeira.

###### Esta área é conhecida como o valor p #####

# O valor-p é a chance de obter uma estatística de teste mais extrema assumindo que a hipótese nula seja verdadeira.
# É comparado com o nível alfa, um limite definido pelo usuário para o erro Tipo I (concluindo um falso positivo).
# O nível alfa define o padrão para quão extremos os dados devem ser para você rejeitar sua hipótese nula.
# É a quantidade de "acaso" que você está disposto a aceitar a fim de alcançar uma estatística de teste mais extrema.
# Uma quantidade baixa significa que você exige uma baixa ocorrência em seu experimento sendo devido ao acaso.
# Em geral o nível alfa é mais comumente fixado em 5%.


#####-------------- Determinando o Resultado --------------------

# Se o nosso valor-p é maior do que o nosso alfa, isso significa que nós falhamos em rejeitar a hipótese nula e concluímos que a
# estatística de teste que observamos pode ter sido devido ao acaso.
# Se o nosso valor-p é menor do que o nosso alfa, isso significa que nós rejeitamos a hipótese nula e concluímos que a estatística
# de teste que observamos foi muito provavelmente não devido ao acaso 

# Valor alfa padrão = 0.05

################## Resultado da análise do Teste A/B ##########################
# No nosso exemplo, falhamos em rejeitar a hipótese nula e concluímos que a maior conversão na landing page B 
# pode ter sido devido ao acaso.


#####-------------- Intervalo de Confiança e Funções R --------------------

# Os intervalos de confiança fornecem uma alternativa para relatar a estatística de teste e fornecer uma medida sumária 
# da incerteza da estatística de teste.

# Calculamos manualmente o intervalo de confiança de 95%:

pnorm(1.96, lower.tail = T)
upper_bound = (b_proportion - a_proportion) + 1.96*SError
lower_bound = (b_proportion - a_proportion) - 1.96*SError

upper_bound
lower_bound

# 1.96 é o z-score associado com um nível de confiança de 95%
# Muitas pessoas incorretamente assumem que um intervalo de confiança de 95% significa que há uma probabilidade de 95% de 
# nosso verdadeiro parâmetro de população estar entre os dois limites.
# Intervalos de confiança são uma medida de incerteza que vem com o método de amostragem. Em amostras repetidas, alguns intervalos
# poderiam incluir o verdadeiro parâmetro da população e outros não, sendo que 95% dos intervalos calculados 
# incluiria o verdadeiro parâmetro da população. Essa é a correta interpretação do intervalo de confiança.

# Podemos executar muito mais rapidamente as análises estatísticas usando funções que existem em R.

# A função t.test () em R pode executar rapidamente a análise e reconhece amostras com tamanho maior que 30,
# usando assim uma distribuição normal.
# Observe que o valor-p é igual ao valor p da saída em nosso cálculo

t.test(b_obs, a_obs, alternative = "greater")


