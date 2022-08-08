# Estudo de Caso - Limpando, Transformando e Manipulando Dados de Voos - 28/05/2022


# Configurando o diretório de trabalho
setwd("C:/Users/deeww/OneDrive/Área de Trabalho/Ciencia de dados/Curso Formação Cientista de dados/BigData_R_Azure_ML/cap 07")
getwd()


# Instalando pacote hflights (Dados de voos de Houston)
install.packages("hflights")
library(hflights)
library(dplyr)
?hflights ##(227.496 linhas e 21 colunas)


# Criando um objeto tbl (tabela - tilbble)
?tbl_df
flights <- tbl_df(hflights)
flights
View(flights)


# Resumindo os dados
str(hflights)   ### para visualizar o tipo de dados
glimpse(hflights)


# Visualizando como dataframe
data.frame(head(flights))
View(flights)


# Filtrando os dados com slice
# Nesse caso, retona todas as linhas que satisfazem a regra (flights$Month == 1 e flights$DayofMonth == 1 e todas as colunas)
flights[1,1] ##busca a primeira célula (linha x coluna)
flights[flights$Month == 1 & flights$DayofMonth == 1, ]


# Aplicando filter (faz o que o slice faz...)
filter(flights, Month == 1, DayofMonth == 1)
filter(flights, UniqueCarrier == "AA" | UniqueCarrier == "UA")   ### '|' significa 'ou'
filter(flights, UniqueCarrier %in% c("AA", "UA"))   


# Select  (Range de coluna de year até DayouMonth); Também as colunas com os caracteres taxi e Delay
select(flights, Year:DayofMonth, contains("Taxi"), contains("Delay")) 

# Organizando os dados
flights %>%
  select(UniqueCarrier, DepDelay) %>%   ###Ordena
  arrange(DepDelay)

flights %>%
  select(Distance, AirTime) %>%
  mutate(Speed = Distance/AirTime*60)   ### Cálculo
 
head(with(flights, tapply(ArrDelay, Dest, mean, na.rm = TRUE))) ### Média
head(aggregate(ArrDelay ~ Dest, flights, mean))

flights %>%
  group_by(Month, DayofMonth) %>%
  tally(sort = TRUE)






