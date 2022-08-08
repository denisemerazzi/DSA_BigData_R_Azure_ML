## P1R - Projeto de Classificação: Avaliação de risco de crédito

# Objetivo: Criar um modelo preditivo para classificar o risco de crédito dos clientes de uma instituição bancária.
# Dataset: “German Credit Data”, que são dados reais gerados por um pesquisador da Universidade de Hamburgo, na Alemanha. 
# O dataset contém 1000 observações e 20 variáveis contendo os dados dos clientes. 
# Disponível em: [https://archive.ics.uci.edu/ml/datasets/Stalog+(German+Credit+Data)](https://archive.ics.uci.edu/ml/datasets/Statlog+(German+Credit+Data))
# Dicionário de dados: (1 = Good, 2 = Bad)
# Nome das variáveis: CheckingAcctStat, Duration, CreditHistory, Purpose, CreditAmount, SavingsBonds, Employment, InstallmentRatePecnt, SexAndStatus, OtherDetorsGuarantors, PresentResidenceTime, Property, Age, OtherInstallments, Housing, ExistingCreditsAtBank, Job, NumberDependents, Telephone, ForeignWorker, CreditStatus

# Configurando o diretório de trabalho
setwd("C:/Users/deeww/OneDrive/Área de Trabalho/Ciencia de dados/Curso Formação Cientista de dados/BigData_R_Azure_ML/cap15")
getwd()


# Variável que controla a execução do script no Azure ML Studio
Azure <- FALSE

if(Azure){
  source("src/ClassTools.R")
  Credit <- maml.mapInputPort(1)
}else{
  source("src/ClassTools.R")
  Credit <- read.csv("credito.csv", header = F, stringsAsFactors = F )
  metaFrame <- data.frame(colNames, isOrdered, I(factOrder))
  Credit <- fact.set(Credit, metaFrame)
  
  # Balancear o número de casos positivos e negativos
  Credit <- equ.Frame(Credit, 2)
}

#Script para checar as colunas do dataset

# Carrega o dataset antes da transformacao: Edição de metadados
# Checando as colunas do dataset
df <- read.csv("credito.csv")
View(df)
str(df)

# Transformando variáveis numéricas em variáveis categóricas (variáveis qualitativas em tipo fator)
toFactors <- c("Duration", "CreditAmount", "Age")
maxVals <- c(100, 1000000, 100)
facNames <- unlist(lapply(toFactors, function(x) paste(x, "_f", sep = "")))
Credit[, facNames] <- Map(function(x, y) quantize.num(Credit[, x], maxval = y), toFactors, maxVals)

str(Credit)

### Variável que controla a execução do script no Azure ML Studio
Azure <- FALSE
if(Azure){
  source("src/ClassTools.R")
  Credit <- maml.mapInputPort(1)
}

# Análise exploratória utilizando gráficos do pacote ggplot2
library(ggplot2)
## Análise dos gráficos que mostram a frequencia de crédito bom/ruim de acordo com as variáveis.
## Exemplo: idade, trabalho...
lapply(colNames2, function(x){
  if(is.factor(Credit[,x])) {
    ggplot(Credit, aes_string(x)) +
      geom_bar() + 
      facet_grid(. ~ CreditStatus) + 
      ggtitle(paste("Total de Credito Bom/Ruim por",x))}})

# Plots CreditStatus vs CheckingAcctStat
# Análise de Idade, status X (bom ou ruim pagador)
lapply(colNames2, function(x){
  if(is.factor(Credit[,x]) & x != "CheckingAcctStat") {
    ggplot(Credit, aes(CheckingAcctStat)) +
      geom_bar() + 
      facet_grid(paste(x, " ~ CreditStatus"))+ 
      ggtitle(paste("Total de Credito Bom/Ruim CheckingAcctStat e",x))
  }})

# Modelo randomForest para criar um plot de importância de variáveis
## Aqui o pacote randomForest é utilizado na seleção das melhores variáveis (variáveis preditoras)
library(randomForest)
modelo <- randomForest( CreditStatus ~ .
                        - Duration
                        - Age
                        - CreditAmount
                        - ForeignWorker
                        - NumberDependents
                        - Telephone
                        - ExistingCreditsAtBank
                        - PresentResidenceTime
                        - Job
                        - Housing
                        - SexAndStatus
                        - InstallmentRatePecnt
                        - OtherDetorsGuarantors
                        - Age_f
                        - OtherInstalments, 
                        data = Credit, 
                        ntree = 100, nodesize = 10, importance = T)

varImpPlot(modelo)
outFrame <- serList(list(credit.model = modelo))

## Criando o modelo
# Cross Tabulation
?table
table(Credit$CreditStatus)

# Funcao para gerar dados de treino e dados de teste (split)
splitData <- function(dataframe, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  index <- 1:nrow(dataframe)
  trainindex <- sample(index, trunc(length(index)/2))
  trainset <- dataframe[trainindex, ]
  testset <- dataframe[-trainindex, ]
  list(trainset = trainset, testset = testset)
}

# Gerando dados de treino e de teste
splits <- splitData(Credit, seed = 808)

# Separando os dados
dados_treino <- splits$trainset
dados_teste <- splits$testset

# Verificando o numero de linhas
nrow(dados_treino)
nrow(dados_teste)

# Construindo o modelo
modelo <- randomForest( CreditStatus ~ CheckingAcctStat
                        + Duration_f
                        + Purpose
                        + CreditHistory
                        + SavingsBonds
                        + Employment
                        + CreditAmount_f, 
                        data = dados_treino, 
                        ntree = 100, 
                        nodesize = 10)

# Imprimindo o resultado
print(modelo)

##Score do modelo: Previsões
# Previsões com o modelo de classificação ramdomForest
require(randomForest)

# Gerando previsões nos dados de teste
previsoes <- data.frame(observado = dados_teste$CreditStatus,
                        previsto = predict(modelo, newdata = dados_teste))


# Visualizando o resultado
View(previsoes)
View(dados_teste)

###Avaliação do modelo
## Avaliação da Performance: Matrix confusion
# Medidas de performance:
# a) Accuracy: Total de resultados corretos/Total de casos analisados
# b) Recall: Total de resultados positivos/total de resultados corretos
# c) Precision: Proporção de ‘true’/total de resultados corretos
# d) F-score: é o balanceamento entre Precision e Recall
# Formulas:
Accuracy <- function(x){
  (x[1,1] + x[2,2]) / (x[1,1] + x[1,2] + x[2,1] + x[2,2])
}

Recall <- function(x){  
  x[1,1] / (x[1,1] + x[1,2])
}

Precision <- function(x){
  x[1,1] / (x[1,1] + x[2,1])
}

W_Accuracy  <- function(x){
  (x[1,1] + x[2,2]) / (x[1,1] + 5 * x[1,2] + x[2,1] + x[2,2])
}

F1 <- function(x){
  2 * x[1,1] / (2 * x[1,1] + x[1,2] + x[2,1])
}

# Criando a confusion matrix.
confMat <- matrix(unlist(Map(function(x, y){sum(ifelse(previsoes[, 1] == x & previsoes[, 2] == y, 1, 0) )},
                             c(2, 1, 2, 1), c(2, 2, 1, 1))), nrow = 2)


# Criando um dataframe com as estatisticas dos testes
df_mat <- data.frame(Category = c("Credito Ruim", "Credito Bom"),
                     Classificado_como_ruim = c(confMat[1,1], confMat[2,1]),
                     Classificado_como_bom = c(confMat[1,2], confMat[2,2]),
                     Accuracy_Recall = c(Accuracy(confMat), Recall(confMat)),
                     Precision_WAcc = c(Precision(confMat), W_Accuracy(confMat)))

print(df_mat)

# Gerando uma curva ROC em R
install.packages("ROCR")
library("ROCR")

# Gerando as classes de dados
class1 <- predict(modelo, newdata = dados_teste, type = 'prob')
class2 <- dados_teste$CreditStatus

# Gerando a curva ROC
?prediction
?performance
pred <- prediction(class1[,2], class2)
perf <- performance(pred, "tpr","fpr") 
plot(perf, col = rainbow(10))

# Gerando Confusion Matrix com o Caret
library(caret)
?confusionMatrix
confusionMatrix(previsoes$observado, previsoes$previsto)


##Tentativa de otimização do modelo utilizando  Modelo randomForest ponderado
# O pacote C50 permite acrescentar peso aos erros, construindo assim um resultado ponderado
install.packages("C50")
library(C50)

# Criando uma Cost Function
Cost_func <- matrix(c(0, 1.5, 1, 0), nrow = 2, dimnames = list(c("1", "2"), c("1", "2")))

# Criando o Modelo
?randomForest
?C5.0

# Criando o modelo
modelo_v2  <- C5.0(CreditStatus ~ CheckingAcctStat
                   + Purpose
                   + CreditHistory
                   + SavingsBonds
                   + Employment,
                   data = dados_treino,  
                   trials = 100,
                   cost = Cost_func)

print(modelo_v2)


# Dataframes com valores observados e previstos
previsoes_v2 <- data.frame(observado = dados_teste$CreditStatus,
                           previsto = predict(object = modelo_v2, newdata = dados_teste))

# Calculando a Confusion Matrix em R (existem outras formas). 
# Label 1 - Credito Ruim
# Label 2 - Credito Bom

# Formulas
Accuracy <- function(x){
  (x[1,1] + x[2,2]) / (x[1,1] + x[1,2] + x[2,1] + x[2,2])
}

Recall <- function(x){  
  x[1,1] / (x[1,1] + x[1,2])
}

Precision <- function(x){
  x[1,1] / (x[1,1] + x[2,1])
}

W_Accuracy  <- function(x){
  (x[1,1] + x[2,2]) / (x[1,1] + 5 * x[1,2] + x[2,1] + x[2,2])
}

F1 <- function(x){
  2 * x[1,1] / (2 * x[1,1] + x[1,2] + x[2,1])
}

# Criando a confusion matrix.
confMat_v2 <- matrix(unlist(Map(function(x, y){sum(ifelse(previsoes_v2[, 1] == x & previsoes_v2[, 2] == y, 1, 0) )},
                                c(2, 1, 2, 1), c(2, 2, 1, 1))), nrow = 2)


# Criando um dataframe com as estatisticas dos testes
df_mat <- data.frame(Category = c("Credito Ruim", "Credito Bom"),
                     Classificado_como_ruim = c(confMat_v2[1,1], confMat_v2[2,1]),
                     Classificado_como_bom = c(confMat_v2[1,2], confMat_v2[2,2]),
                     Accuracy_Recall = c(Accuracy(confMat_v2), Recall(confMat_v2)),
                     Precision_WAcc = c(Precision(confMat_v2), W_Accuracy(confMat_v2)))

print(df_mat)

# Gerando Confusion Matrix com o pacote Caret
library(caret)
confusionMatrix(previsoes_v2$observado, previsoes_v2$previsto)


##Avaliação do modelo
# Usando o dplyr para filter linhas com classificação incorreta
require(dplyr)
creditTest <- cbind(dados_teste, scored = compFrame[ ,2] )
creditTest <- creditTest %>% filter(CreditStatus != scored)

# Plot dos residuos para os niveis de cada fator
require(ggplot2)
colNames <- c("CheckingAcctStat", "Duration_f", "Purpose",
              "CreditHistory", "SavingsBonds", "Employment",
              "CreditAmount_f", "Employment")

lapply(colNames, function(x){
  if(is.factor(creditTest[,x])) {
    ggplot(creditTest, aes_string(x)) +
      geom_bar() + 
      facet_grid(. ~ CreditStatus) + 
      ggtitle(paste("Numero de creditos ruim/bom por",x))}})


# Plot dos residuos condicionados nas variáveis CreditStatus vs CheckingAcctStat
lapply(colNames, function(x){
  if(is.factor(creditTest[,x]) & x != "CheckingAcctStat") {
    ggplot(creditTest, aes(CheckingAcctStat)) +
      geom_bar() + 
      facet_grid(paste(x, " ~ CreditStatus"))+ 
      ggtitle(paste("Numero de creditos bom/ruim por CheckingAcctStat e ",x))
  }})






