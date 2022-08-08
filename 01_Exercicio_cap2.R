#Exercícios - Capítulo 2 - Fundamentos da linguagem R - Realizados em 06/05/2022

# Exercício 1 - Crie um vetor com 30 números inteiros

vetorinteiros = c(10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 110, 120, 130, 140, 150, 160, 170, 180, 190, 200,210, 220, 230, 240, 250, 260, 280, 290, 300) 
#ou
vec <- c(1:30)


# Exercício 2 - Crie uma matriz com 4 linhas e 4 colunas preenchida com números inteiros

mat1 <- matrix (c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16), nr = 4)
mat1

# ou
mat <- matrix(c(1:16), nrow = 4, ncol = 4)

# Exercício 3 - Crie uma lista unindo o vetor e matriz criados anteriormente

uniao = cbind(mat1, vetorinteiros)
uniao

# ou
lst <- list(vec,mat)
lst

# Exercício 4 - Usando a função read.table() leia o arquivo do link abaixo para um dataframe
# http://data.princeton.edu/wws509/datasets/effort.dat

dataset <- data.frame(read.csv(file = 'exercicio.csv', header = TRUE, sep = ","))
dataset

# ou
df <- data.frame(read.table("http://data.princeton.edu/wws509/datasets/effort.dat"))
class(df)
df

# Exercício 5 - Transforme o dataframe anterior, em um dataframe nomeado com os seguintes labels:
# c("config", "esfc", "chang")

names(df) = c("config", "esfc", "chang")
df


# Exercício 6 - Imprima na tela o dataframe iris, verifique quantas dimensÃµes existem no dataframe iris e imprima um resumo do dataset
iris
View(iris)
head(iris)
dim(iris)
summary(iris)

# Exercício 7 - Crie um plot simples usando as duas primeiras colunas do dataframe iris

plot(iris$Sepal.Length, iris$Sepal.Width)

# Exercício 8 - Usando a funÃ§Ã£o subset, crie um novo dataframe com o conjunto de dados do dataframe iris em que Sepal.Length > 7
# Dica: consulte o help para aprender como usar a funÃ§Ã£o subset()

help("subset")
?subset
iris1 = subset(iris, Sepal.Length  > 7)
View(iris1)

# Exercícios 9 (Desafio) - Crie um dataframe que seja cópi do dataframe iris e usando a função slice(), divida o dataframe em um subset de 15 linhas
# Dica 1: Você vai ter que instalar e carregar o pacote dplyr
# Dica 2: Consulte o help para aprender como usar a função slice()

novo_iris <- iris
novo_iris
install.packages("dplyr")
library(dplyr)
?slice
slice(novo_iris, 1:15)
class(slice(novo_iris, 1:15))

# Exercícios 10 - Use a função filter no seu novo dataframe criado no item anterior e retorne apenas valores em que Sepal.Length > 6
# Dica: Use o RSiteSearch para aprender como usar a função filter
RSiteSearch('filter')
filter(novo_iris,Sepal.Length > 6)
