# Mini-projeto 1 : Análise exploratória socioeconômica

# Questões:
# 1. O aumento do PIB per capita de um país afeta positivamente a expectativa de vida 
#dos cidadãos ao nascer? Qual a correlação entre essas duas variáveis?

# 2. Existe uma correlação entre a escala de vida e a conscientização do público em geral 
# sobre a corrupção nos negócios e no governo? Qual a correlação entre essas duas variáveis?

#3. O aumento na escala de vida tem algum efeito na média de felicidade entre o público 
#em geral? Qual a correlação entre essas duas variáveis?

#4. O país com o menor índice de suporte social tem maior percepção de corrupção em 
#relação às empresas e ao governo no país?

#5. Pessoas generosas são mais felizes?

#Organizando o diretório de trabalho:
setwd("C:/Users/deeww/OneDrive/Área de Trabalho/Ciencia de dados/Curso Formação Cientista de dados/BigData_R_Azure_ML/Miniprojeto1")
getwd()

library(dplyr)
library(ggplot2)

#Carregando e observando os dados:
dados <- read.csv("dataset.csv")
View(dados)
str(dados)
dim(dados)
summary(dados)

# Dicionário de dados:
#data.frame:	1949 obs. of  11 variables:
#Country.name
#year
#Life.Ladder ('escada de vida')
#Log.GDP.per.capita (PIB per capita transformada com logaritmo)
#Social.support (apoio social)
#Healthy.life.expectancy.at.birth (expectativa de vida saudável ao nascer)
#Freedom.to.make.life.choices(liberdade para fazer escolhas)
#Generosity
#Perceptions.of.corruption
#Positive.affect (afeta positivamente)
#Negative.affect (afeta negativamente)


####Análise exploratória e limpeza dos dados####

# Quantas linhas tem casos completos? (Função complete.cases)
complete_cases <- sum(complete.cases(dados))

# Quantas linhas tem casos incompletos?
not_complete_cases <- sum(!complete.cases(dados))

# Qual o percentual de dados incompletos?
percentual <- (not_complete_cases / complete_cases) * 100
percentual

# Remove os objetos anteriores para liberar memória RAM
rm(complete_cases)
rm(not_complete_cases)

# Nomes das colunas
colnames(dados)

# Grava os nomes das colunas em um vetor
myColumns <- colnames(dados)
myColumns

# Vamos renomear as colunas para facilitar nosso trabalho mais tarde
myColumns[1] <- "NomePais"
myColumns[2] <- "Ano"
myColumns[3] <- "IndicadorNivelVida"
myColumns[4] <- "PIB_Per_Capita"
myColumns[5] <- "SuporteSocial"
myColumns[6] <- "ExpectativaVida"
myColumns[7] <- "IndicadorLiberdade"
myColumns[8] <- "IndicadorGenerosidade"
myColumns[9] <- "IndicadorCorrupcao"
myColumns[10] <- "IndicadorEmocoesPositivas"
myColumns[11] <- "IndicadorEmocoesNegativas"

# Verifica o resultado
myColumns

# Atribui os novos nomes de colunas ao dataframe
colnames(dados) <- myColumns
rm(myColumns)

# Visualiza os dados
View(dados)

# Verificando quantos países foram incluídos na coleta de dados
length(unique(dados$NomePais))

# Lista os países únicos e grava o resultado (antes de remover registros com valores NA)
list_countries_with_na <- unique(dados$NomePais)
list_countries_with_na

# Vamos eliminar linhas com valores NA
dados <- na.omit(dados)

# Dimensões
dim(dados)

# Lista de países após remover valores NA
list_of_countries_without_na <- unique(dados$NomePais)
list_of_countries_without_na

# Verificando se perdemos países ao remover valores NA
length(list_countries_with_na)
length(list_of_countries_without_na)

# Verificando a diferença antes e depois de remover valores NA
setdiff(list_countries_with_na, list_of_countries_without_na)

# Remove os objetos
rm(list_countries_with_na)
rm(list_of_countries_without_na)

# Verificando quais anos estão presentes nos dados
anos <- unique(dados$Ano)
range(anos)
length(unique(dados$Ano))
rm(anos)

# Número de registros por ano
table(dados$Ano)

# Vamos remover os anos com menor contribuição (menor volume de dados, manos que 100 registros)
dados_por_anos <- dados[dados$Ano!=2005 & dados$Ano!=2006 & dados$Ano!=2007 & dados$Ano!=2020,]

# Número de registros por ano (frequência total para cada categoria)
table(dados_por_anos$Ano)

# Extraindo as variáveis numéricas
numeric_variable_list <- sapply(dados, is.numeric)
numerical_data <- dados[numeric_variable_list]

# Matriz de Correlação
cor(numerical_data)

# Correlation Plot
pairs(numerical_data)
pairs(numerical_data[1:5],labels = colnames(numerical_data)[1:5])
pairs(numerical_data[6:10],labels = colnames(numerical_data)[6:10])

##### Análise Exploratória dos Dados - Resposta às Perguntas de Negócio ##### 

##### Parte 1 - Organização dos Dados ##### 

# Vamos realizar a análise considerando a média de indicadores por país.
# Calculamos as médias fazendo agrupamento por indicador e concatenamos os dataframes resultantes.

# Visualiza os dados
View(dados)

# Nomes das colunas
colnames(dados)

##Calculando e agrupando as médias de cada país, 
# utiliza-se todos os indicadores (mas o tempo ficou de fora do dataframe)


# Agrupando os dados e calculando média por país
pib_per_capita_pais_media <- dados %>%
  group_by(NomePais) %>%
  summarize(PIB_Per_Capita = mean(PIB_Per_Capita))

# Agrupando os dados e calculando média por país
suporte_social_pais_media <- dados %>%
  group_by(NomePais) %>%
  summarize(SuporteSocial = mean(SuporteSocial))

# Merge dos dataframes
df_medias <- merge(pib_per_capita_pais_media, suporte_social_pais_media)
View(df_medias)

# Remova o que não estiver mais usando
rm(pib_per_capita_pais_media)
rm(suporte_social_pais_media)

# Agrupando os dados e calculando média por país
ind_nivel_vida_pais_media <- dados %>%
  group_by(NomePais) %>%
  summarize(IndicadorNivelVida = mean(IndicadorNivelVida))

# Merge
df_medias <- merge(df_medias, ind_nivel_vida_pais_media)
View(df_medias)
rm(ind_nivel_vida_pais_media)

# Agrupando os dados e calculando média por país
expectativa_vida_pais_media <- dados %>%
  group_by(NomePais) %>%
  summarize(ExpectativaVida = mean(ExpectativaVida))

# Merge
df_medias <- merge(df_medias, expectativa_vida_pais_media)
View(df_medias)
rm(expectativa_vida_pais_media)

# Agrupando os dados e calculando média por país
ind_liberdade_pais_media <- dados %>%
  group_by(NomePais) %>%
  summarize(IndicadorLiberdade = mean(IndicadorLiberdade))

df_medias <- merge(df_medias, ind_liberdade_pais_media)
View(df_medias)
rm(ind_liberdade_pais_media)

# Agrupando os dados e calculando média por país
ind_generosidade_pais_media <- dados %>%
  group_by(NomePais) %>%
  summarize(IndicadorGenerosidade = mean(IndicadorGenerosidade))

# Merge
df_medias <- merge(df_medias, ind_generosidade_pais_media)
View(df_medias)
rm(ind_generosidade_pais_media)

# Agrupando os dados e calculando média por país
ind_corrupcao_pais_media <- dados %>%
  group_by(NomePais) %>%
  summarize(IndicadorCorrupcao = mean(IndicadorCorrupcao))

# Merge
df_medias <- merge(df_medias, ind_corrupcao_pais_media)
View(df_medias)
rm(ind_corrupcao_pais_media)

# Agrupando os dados e calculando média por país
ind_pos_pais_media <- dados %>%
  group_by(NomePais) %>%
  summarize(IndicadorEmocoesPositivas = mean(IndicadorEmocoesPositivas))

# Merge
df_medias <- merge(df_medias, ind_pos_pais_media)
View(df_medias)
rm(ind_pos_pais_media)

# Agrupando os dados e calculando média por país
ind_neg_pais_media <- dados %>%
  group_by(NomePais) %>%
  summarize(IndicadorEmocoesNegativas = mean(IndicadorEmocoesNegativas))

# Merge
df_medias <- merge(df_medias, ind_neg_pais_media)
View(df_medias)
rm(ind_neg_pais_media)
dim(df_medias)

##### Parte 2 - Plots e Estatísticas ##### 

# Dados
colnames(df_medias)
View(df_medias)
str(df_medias)

# Pergunta 1
# O aumento do PIB per capita de um país afeta positivamente a expectativa de vida dos cidadãos ao nascer?
# Qual a correlação entre essas duas variáveis?
plot(df_medias$PIB_Per_Capita, df_medias$ExpectativaVida)
cor.test(df_medias$PIB_Per_Capita, df_medias$ExpectativaVida, method = "pearson")
#correlação = 0.8537768 (positiva: aumenta o PIB, aumenta a expectativa de vida)

# Pergunta 2
# Existe uma correlação entre a escala de vida e a conscientização do público em geral sobre a corrupção 
# nos negócios e no governo? 
# Qual a correlação entre essas duas variáveis?
plot(df_medias$IndicadorNivelVida, df_medias$IndicadorCorrupcao)
cor.test(df_medias$IndicadorNivelVida, df_medias$IndicadorCorrupcao, method = "pearson")
#correlação = -0.4642275 (negativa: aumenta o nivel de vida, diminui a corrupção)

# Pergunta 3
# O aumento na escala de vida tem algum efeito na média de felicidade entre o público em geral?
# Qual a correlação entre essas duas variáveis?
plot(df_medias$IndicadorNivelVida, df_medias$IndicadorEmocoesPositivas)
cor.test(df_medias$IndicadorNivelVida, df_medias$IndicadorEmocoesPositivas, method = "pearson")
#Correlação = 0.5778006 (positiva)


# Pergunta 4
# O país com o menor índice de suporte social tem maior percepção de corrupção em relação 
# às empresas e ao governo no país?

# Indicadores
df_medias[df_medias$SuporteSocial == min(df_medias$SuporteSocial),]
df1 <- df_medias[df_medias$NomePais == "Central African Republic",]
View(df1)
df1$SuporteSocial
df1$IndicadorCorrupcao
max(df_medias$SuporteSocial)
max(df_medias$IndicadorCorrupcao)
## O país com menor apoio social é Central African Republic


# Plot e Estatísticas
df2 <- dados[dados$NomePais == "Central African Republic",]
View(df2)
plot(df2$SuporteSocial, df2$IndicadorEmocoesPositivas)
cor.test(df2$SuporteSocial, df2$IndicadorEmocoesPositivas, method = "pearson")
#Correlação negativa (-0.49): pouco apoio social e alta percepção de corrupção

# Pergunta 5
# Pessoas generosas são mais felizes?
plot(df_medias$IndicadorGenerosidade, df_medias$IndicadorEmocoesPositivas)
cor.test(df_medias$IndicadorGenerosidade, df_medias$IndicadorEmocoesPositivas, method = "pearson")
#correlação positiva (0.39): aumenta a  generosidade, aumenta a felicidade
