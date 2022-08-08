#Lista de ExercÃ­cios - CapÃ­tulo 10 - 06/07/2022

# Configurando o diretÃ³rio de trabalho

setwd("C:/Users/deeww/OneDrive/Ãrea de Trabalho/Ciencia de dados/Curso FormaÃ§Ã£o Cientista de dados/BigData_R_Azure_ML/cap10")
getwd()


# Pacotes
install.packages("dplyr")  ## manipulaÃ§Ã£o de dados
install.packages('nycflights13')  ## pacotes de dados de voo
library('ggplot2')  ### construÃ§Ã£o de graficos
library('dplyr')
library('nycflights13')
View(flights)  ##dep_delay: atraso na partida, arr_delay:atraso de chegada, carrier: companhia aerea
?flights

# Definindo o Problema de NegÃ³cio
# Crie um teste de hipÃ³tese para verificar se os voos da Delta Airlines (DL)
# atrasam mais do que os voos da UA (United Airlines)


##### ATENÃÃO #####
# VocÃª vai precisar do conhecimento adquirido em outros capÃ­tulos do curso 
# estudados atÃ© aqui para resolver esta lista de exercÃ­cios!


# ExercÃ­cio 1 - Construa o dataset pop_data com os dados de voos das 
# companhias aÃ©reas UA (United Airlines) e DL (Delta Airlines). 
# O dataset deve conter apenas duas colunas, nome da companhia e atraso nos voos de chegada.
# Os dados devem ser extraÃ­dos do dataset flights para construir o dataset pop_data
# Vamos considerar este dataset como sendo nossa populaÃ§Ã£o de voos
## CraiaÃ§Ã£o de um dataset
pop_data = na.omit(flights) %>% 
  filter(carrier == 'UA' | carrier == 'DL', arr_delay >= 0) %>% ##filtrando a coluna UA e DL
  select(carrier, arr_delay) %>%  ##Colunas
  group_by(carrier) %>%
  sample_n(17000) %>%   ### amostra com 17000
  ungroup()

View(pop_data)

?ungroup()   ## remove os valores NA, caso existam, utiliza-se junto Ã  funÃ§Ã£o group_by (pacote dplyr)



# ExercÃ­cio 2  - Crie duas amostras de 1000 observaÃ§Ãµes cada uma a partir do 
# dataset pop_data apenas com dados da companhia DL para amostra 1 e apenas dados 
# da companhia UA na amostra 2

# Dica: inclua uma coluna chamada sample_id preenchida com nÃºmero 1 para a primeira 
# amostra e 2 para a segunda amostra

amostra1 = na.omit(pop_data) %>% 
  select(carrier, arr_delay) %>%
  filter(carrier == 'DL') %>%
  mutate(sample_id = '1') %>%
  sample_n(1000)

View(amostra1)

amostra2 = na.omit(pop_data) %>% 
  select(carrier, arr_delay) %>%
  filter(carrier == 'UA') %>%
  mutate(sample_id = '2') %>%
  sample_n(1000)

View(amostra2)

# ExercÃ­cio 3 - Crie um dataset contendo os dados das 2 amostras criadas no item anterior. 

samples = rbind(amostra1,amostra2)
View(samples)

# ExercÃ­cio 4 - Calcule o intervalo de confianÃ§a (95%) da amostra1
erro_padrao_amostra1 = sd(amostra1$arr_delay) / sqrt(nrow(amostra1))

# Usamos a fÃ³rmula: erro_padrao_amostra1 = sd(amostra1$arr_delay) / sqrt(nrow(amostra1))

# Esta fÃ³rmula Ã© usada para calcular o desvio padrÃ£o de uma distribuiÃ§Ã£o da mÃ©dia amostral
# (de um grande nÃºmero de amostras de uma populaÃ§Ã£o). Em outras palavras, sÃ³ Ã© aplicÃ¡vel 
# quando vocÃª estÃ¡ procurando o desvio padrÃ£o de mÃ©dias calculadas a partir de uma amostra de 
# tamanho nð, tirada de uma populaÃ§Ã£o.

# Digamos que vocÃª obtenha 10000 amostras de uma populaÃ§Ã£o qualquer com um tamanho de amostra de n = 2.
# EntÃ£o calculamos as mÃ©dias de cada uma dessas amostras (teremos 10000 mÃ©dias calculadas).
# A equaÃ§Ã£o acima informa que, com um nÃºmero de amostras grande o suficiente, o desvio padrÃ£o das mÃ©dias 
# da amostra pode ser aproximado usando esta fÃ³rmula: sd(amostra) / sqrt(nrow(amostra))
  
# Deve ser intuitivo que o seu desvio padrÃ£o das mÃ©dias da amostra serÃ¡ muito pequeno, 
# ou em outras palavras, as mÃ©dias de cada amostra terÃ£o muito pouca variaÃ§Ã£o.

# Com determinadas condiÃ§Ãµes de inferÃªncia (nossa amostra Ã© aleatÃ³ria, normal, independente), 
# podemos realmente usar esse cÃ¡lculo de desvio padrÃ£o para estimar o desvio padrÃ£o de nossa populaÃ§Ã£o. 
# Como isso Ã© apenas uma estimativa, Ã© chamado de erro padrÃ£o. A condiÃ§Ã£o para usar isso como 
# uma estimativa Ã© que o tamanho da amostra n Ã© maior que 30 (dado pelo teorema do limite central) 
# e atende a condiÃ§Ã£o de independÃªncia n <= 10% do tamanho da populaÃ§Ã£o.

# Erro padrÃ£o
erro_padrao_amostra1 = sd(amostra1$arr_delay) / sqrt(nrow(amostra1))

# Limites inferior e superior
# 1.96 Ã© o valor de z score para 95% de confianÃ§a
lower = mean(amostra1$arr_delay) - 1.96 * erro_padrao_amostra1  
upper = mean(amostra1$arr_delay) + 1.96 * erro_padrao_amostra1

# Intervalo de confianÃ§a

ic_1 = c(lower,upper)
mean(amostra1$arr_delay)
ic_1

# ExercÃ­cio 5 - Calcule o intervalo de confianÃ§a (95%) da amostra2

erro_padrao_amostra2 = sd(amostra2$arr_delay) / sqrt(nrow(amostra2))
lower = mean(amostra2$arr_delay) - 1.96 * erro_padrao_amostra2
upper = mean(amostra2$arr_delay) + 1.96 * erro_padrao_amostra2
ic_2 = c(lower,upper)
mean(amostra2$arr_delay)
ic_2


# ExercÃ­cio 6 - Crie um plot Visualizando os intervalos de confianÃ§a criados nos itens anteriores
# Dica: Use o geom_point() e geom_errorbar() do pacote ggplot2
## montando o grÃ¡fico de comparaÃ§Ã£o entre as duas amostras.. observando a mÃ©dia
toPlot = summarise(group_by(samples, sample_id), mean = mean(arr_delay))
toPlot = mutate(toPlot, lower = ifelse(toPlot$sample_id == 1,ic_1[1],ic_2[1]))
toPlot = mutate(toPlot, upper = ifelse(toPlot$sample_id == 1,ic_1[2],ic_2[2]))
ggplot(toPlot, aes(x = sample_id, y=mean, colour = sample_id )) + 
  geom_point() +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.1)


View(toPlot)

# ExercÃ­cio 7 - Podemos dizer que muito provavelmente, as amostras vieram da mesma populaÃ§Ã£o? 
# Por que?
##Sim. A maior parte dos dados reside no mesmo intervalo de confianÃ§a nas duas amostras.


# ExercÃ­cio 8 - Crie um teste de hipÃ³tese para verificar se os voos da Delta Airlines (DL)
# atrasam mais do que os voos da UA (United Airlines)

# H0 e H1 devem ser mutuamente exclusivas.

# H0 = NÃ£o hÃ¡ diferenÃ§a significativa entre os atrasos da DL e UA (diff da mÃ©dia de atrasos = 0).
# H1 = Delta atrasa mais (diff das mÃ©dias > 0).


# Cria as amostras (atraso maior que zero)
dl <- sample_n(filter(pop_data, carrier == "DL", arr_delay > 0), 1000)
ua <- sample_n(filter(pop_data, carrier == "UA", arr_delay > 0), 1000)

View(dl)
View(ua)

# Calcula erro padrÃ£o e mÃ©dia
se = sd(dl$arr_delay) / sqrt(nrow(dl))
mean(dl$arr_delay)

# Limites inferior e superior
lower = mean(dl$arr_delay) - 1.96 * se
upper = mean(dl$arr_delay) + 1.96 * se
ic_dl = c(lower,upper)
ic_dl

# Repete o processo para a outra companhia
se = sd(ua$arr_delay) / sqrt(nrow(ua))
mean(ua$arr_delay)

lower = mean(ua$arr_delay) - 1.96 * se
upper = mean(ua$arr_delay) + 1.96 * se
ic_ua = c(lower,upper)
ic_ua

# Teste t
# O teste t (de Student) foi desenvolvido por Willian Sealy Gosset em 1908 que usou o
# pseudÃ´nimo âStudentâ em funÃ§Ã£o da confidencialidade requerida por seu empregador
# (cervejaria Guiness) que considerava o uso de estatÃ­stica na manutenÃ§Ã£o da qualidade como
# uma vantagem competitiva.
# O teste t de Student tem diversas variaÃ§Ãµes de aplicaÃ§Ã£o, e pode ser usado na comparaÃ§Ã£o 
# de duas (e somente duas) mÃ©dias e as variaÃ§Ãµes dizem respeito Ã s hipÃ³teses que sÃ£o testadas

t.test(dl$arr_delay, ua$arr_delay, alternative="greater")


##t (teste student), df (grau de liberdade), p-value(valor p), 
##

# Valor p
# O valor-p Ã© uma quantificaÃ§Ã£o da probabilidade de se errar ao rejeitar H0 e a mesma
# decorre da distribuiÃ§Ã£o estatÃ­stica adotada.
# Se o valor-p Ã© menor que o nÃ­vel de significÃ¢ncia, conclui-se que o correto Ã© rejeitar a
# hipÃ³tese de nulidade.

# Valor p Ã© a probabiblidade de que a estatÃ­stica do teste assuma um valor extremo em relaÃ§Ã£o 
# ao valor observado quando H0 Ã© verdadeira.

# Estamos trabalhando com alfa igual a 0.05 (95% de confianÃ§a)

# Regra
# Baixo valor p: forte evidÃªncia empÃ­rica contra h0
# Alto valor p: pouca ou nenhuma evidÃªncia empÃ­rica contra h0

# Falhamos em rejeitar a hipÃ³tese nula, pois p-valor Ã© maior que o nÃ­vel de significÃ¢ncia
# Isso que dizer que hÃ¡ uma probabilidade alta de nÃ£o haver diferenÃ§a significativa entre os atrasos.
# Para os nossos dados, nÃ£o hÃ¡ evidÃªncia estatÃ­stica de que a DL atrase mais que a UA.
###p Ã© maior que alfa:hÃ¡ uma alta probabilidade de nÃ£o haver diferenÃ§a significativa entre os atraso das duas companhias aÃ©reas.
##Falhamos em rejeitar H0 - nÃ£o hÃ¡ diferenÃ§a entre os atrasos
##conjunto de regras estatÃ­sticas
