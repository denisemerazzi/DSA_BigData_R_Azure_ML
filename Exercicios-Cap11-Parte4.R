# Lista de Exercícios Parte 4 - Capítulo 11


# Configurando o diretório de trabalho
setwd("C:/Users/deeww/OneDrive/Área de Trabalho/Ciencia de dados/Curso Formação Cientista de dados/BigData_R_Azure_ML/cap11")
getwd()


# Definindo o Problema: OCR - Optical Character Recognition
# Seu modelo deve prever o caracter a partir do dataset fornecido. Use um modelo SVM(suport vector machine)
# SVM é melhor que  rede neurl, porém não é muito prático e muito complexo

## Explorando e preparando os dados
letters <- read.csv("letterdata.csv")
View(letters)
str(letters)

# Criando dados de treino e dados de teste
letters_treino <- letters[1:16000, ]
letters_teste  <- letters[16001:20000, ]

## Treinando o Modelo
## o pacote kernlab, tras a função ksvm
install.packages("kernlab")
library(kernlab)

# Criando dados de treino e dados de teste (matriz)
# O ideal é uma divisão aleatória
letters_treino <- letters[1:16000, ]
letters_teste  <- letters[16001:20000, ]

## Treinando o Modelo
install.packages("kernlab")
library(kernlab)

# Criando o modelo com o kernel vanilladot
# Obs: Converta a primeira variável do dataset para o tipo fator:
letters_treino$letter <- as.factor(letters_treino$letter)
str(letters_treino)
letter_classifier <- ksvm(letter ~ ., data = letters_treino, kernel = "vanilladot")

# Visualizando resultado do modelo
letter_classifier

# Avaliando a performance do modelo
letter_predictions <- predict(letter_classifier, letters_teste)
head(letter_predictions)
table(letter_predictions, letters_teste$letter)
##Essa função table trás uma matriz de confusão... dizendo quantas vezes o modelo acertou e errou as letras 

# Criando um vetor de TRUE/FALSE indicando previsoes corretas/incorretas (quanto acertou, quanto errou)
agreement <- letter_predictions == letters_teste$letter
table(agreement)
prop.table(table(agreement))

## Otimizando o Modelo
set.seed(12345)

# Recriando o modelo com outro tipo de kernel
letter_classifier_rbf <- ksvm(letter ~ ., data = letters_treino, kernel = "rbfdot")

# Novas previsoes
letter_predictions_rbf <- predict(letter_classifier_rbf, letters_teste)

# Compare os resultados com a primeira versao do modelo
agreement_rbf <- letter_predictions_rbf == letters_teste$letter
table(agreement_rbf)
prop.table(table(agreement_rbf))






