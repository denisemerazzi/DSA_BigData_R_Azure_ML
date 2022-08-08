# Lista de Exercícios - Capítulo 4 - Realizado em 20/05/2022


# Configurando o diretório de trabalho

setwd("C:/Users/deeww/OneDrive/Área de Trabalho/Ciencia de dados/Curso Formação Cientista de dados/BigData_R_Azure_ML/cap03")
getwd()

# Exercicio 1 - Crie uma função que receba os dois vetores abaixo como parâmetro, 
# converta-os em um dataframe e imprima no console
vec1 <- (10:13)
vec2 <- c("a", "b", "c", "d")

myfunction <- function(vec1, vec2){
  df = data.frame(cbind(vec1, vec2))
  print(df)
}

myfunction(vec1, vec2)

# Exercicio 2 - Crie uma matriz com 4 linhas e 4 colunas preenchida com 
# números inteiros e calcule a média de cada linha
  
mat <- matrix(c(1:16), nrow = 4, ncol = 4) 
mat
apply(mat, 1, mean)

# Exercicio 3 - Considere o dataframe abaixo. 
# Calcule a média por disciplina e depois calcule a média de apenas uma disciplina
escola <- data.frame(Aluno = c('Alan', 'Alice', 'Alana', 'Aline', 'Alex', 'Ajay'),
                     Matematica = c(90, 80, 85, 87, 56, 79),
                     Geografia = c(100, 78, 86, 90, 98, 67),
                     Quimica = c(76, 56, 89, 90, 100, 87))

escola
View(escola)
apply(escola[, c(2,3,4)], 2, mean)

#Assim dá erro: apply(escola$Matematica, 2, mean), ao tentar a média da coluna matemática,
#Apply espera que o objeto tenha algumas dimensões, é possível fazer
#acrescentando um drop = F (false) ao código
apply(escola[, c(2), drop = F], 2, mean)


# Exercicio 4 - Cria uma lista com 3 elementos, todos numéricos 
# e calcule a soma de todos os elementos da lista
lst <- list (a = 1:10, b = 1:5)
lst
do.call(sum,lst)

# Exercicio 5 - Transforme a lista anterior um vetor
# Transforma um elemento multidimensional e unidimensional

unlist(lst)


# Exercicio 6 - Considere a string abaixo. Substitua a palavra "textos" por "frases"
str <- c("Expressoes", "regulares", "em linguagem R", 
         "permitem a busca de padroes", "e exploracao de textos",
         "podemos buscar padroes em digitos",
         "como por exemplo",
         "10992451280")

gsub("textos", "frases", str)


# Exercicio 7 - Usando o dataset mtcars, crie um grafico com ggplot do tipo 
# scatter plot. Use as colunas disp e mpg nos eixos x e y respectivamente

library(ggplot2)
head(mtcars)
ggplot(data = mtcars, aes(x = disp, y = mpg)) + geom_point()

# outra solução
ggplot(data = mtcars, 
       aes(x = disp, y = mpg, 
           colour = as.factor(am))) + geom_point()

# Exercicio 8 - Considere a matriz abaixo.
# Crie um bar plot que represente os dados em barras individuais.
mat1 <- matrix(c(652,1537,598,242,36,46,38,21,218,327,106,67), nrow = 3, byrow = T)
mat1

barplot(mat1, beside = T)

# Exercício 9 - Qual o erro do código abaixo? Troubleshooting (análise de erros por parte)
# fill é um objeto não encontrado, fill não é uma categoria (coluna), no lugar pode ser colocada outra coluna, por exemplo "cut"
data(diamonds)
ggplot(data = diamonds, aes(x = price, group = fill, fill = cut)) + 
  geom_density(adjust = 1.5)



# Exercício 10 - Qual o erro do código abaixo?
# geom_barplot não é uma função do ggplot, é uma função do pacote Lattice
# O nome correto da função é geom_bar

ggplot(mtcars, aes(x = as.factor(cyl), fill = as.factor(cyl))) + 
  geom_barplot() +
  labs(fill = "cyl")




