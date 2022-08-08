# Lista de Exercícios - Capítulo 3 - Realizados em 14/05/2022


setwd("C:/Users/deeww/OneDrive/Área de Trabalho/Ciencia de dados/Curso Formação Cientista de dados/BigData_R_Azure_ML/cap03")
getwd()

# Exercício 1 - Pesquise pela função que permite listar todos os arquivos no diretório de trabalho
list.files()

# Exercício 2 - Crie um dataframe a partir de 3 vetores: um de caracteres, um lógico e um de números

vetor1 <- c("Canoas", "Camaquã", "Esteio")
vetor2 <- c("True", "False", "True")
vetor3 <- c(1,2,3)

df <- data.frame(vetor1, vetor2, vetor3)
df

# Exercício 3 - Considere o vetor abaixo. 
# Crie um loop que verifique se há números maiores que 10 e imprima o número e uma mensagem no console.

# Criando um Vetor
vec1 <- c(12, 3, 4, 19, 34)
vec1

for(i in 1:length(vec1)){
  if (vec1[i] > 10) {
    print(vec1[i])
    print('Este elemento do vetor é maior que 10')
    } else {
      print(vec1[i])
      print("Este elemento do vetor é menor que 10")
    }
}

# Exercício 4 - Considere a lista abaixo. Crie um loop que imprima no console cada elemento da lista
lst2 <- list(2, 3, 5, 7, 11, 13)
lst2

for (i in 1:length(lst2)) {
  print(lst2 [[i]])
}

# Exercício 5 - Considere as duas matrizes abaixo. 
# Faça uma multiplicação element-wise e multiplicação normal entre as materizes
mat1 <- matrix(c(1:50), nrow = 5, ncol = 5, byrow = T)
mat1
mat2 <- t(mat1)
mat2

# Multiplicação element-wise (multiplica por elemento)
mat3 <- mat1 * mat2
mat3

# Multiplicação de matrizes (tradicional)
# Item 1: Multiplica a primeira linha da mat1 com a primeira coluna da mat2
# Item 2: Mutliplica s segunda linha, pela primeira coluna
# Por exemplo: uma matriz 2 x 3  multiplicada por uma matriz 3x1, tem como resultado uma matriz 2x1
# Item 1 [1,1] ==> (1x1)+(2x2)+(3x3)+(4x4)+(5x5) = 55
# Item 2 [2,1] ==> (6x1)+(7x2)+(8x3)+(9x4)+(10x5) = 130
# Item 3 [3,1] ==> (11x1)+(12x2)+(13x3)+(14x4)+(15x5) = 205

mat4 <- mat1 %*% mat2
mat4


# Exercício 6 - Crie um vetor, matriz, lista e dataframe e faça a nomeação de cada um dos objetos

# vetor
vet1 <- c("azul", "vermelho", "laranja", "amarelo", "rosa")
names(vec1) <-c('coluna1','coluna2','coluna3','coluna4','coluna5')

# Lista
lista1 = list('Casaco', 'Blusa', 'Blusa','Vestido','calça')
names(lista1) <-c('registro1','registro2','registro3','registro4','registro5')
lista1

# Matriz
mat1 <- matrix(c(1:10), nrow = 5, ncol=2, byrow = T)
dimnames(mat1) = (list(c("linha1", "linha2", "linha3", "linha4", "linha5"), c("col1", "col2")))
mat1

# Dataframes
Frutas = c("maçã", "banana", "figo", "morango", "abacaxi")
Quantidade_kg = c(3, 1.9, 2, 1, 5)
codigo = c(001, 002, 003, 004, 005)

estoque_frutas = data.frame(Frutas, Quantidade_kg, codigo)
estoque_frutas

#Renomeando:
colnames(estoque_frutas)<- c('Fuits', 'Quant_kg', 'cod')
rownames(estoque_frutas)<- c('fruta1','fruta2','fruta3','fruta4','fruta5')
estoque_frutas


# Exercício 7 - Considere a matriz abaixo. Atribua valores NA de forma aletória para 50 elementos da matriz
# Dica: use a função sample()
mat2 <- matrix(1:90, 10)
mat2

?sample
mat2[sample(1:50, 10)] = NA
mat2

# Exercício 8 - Para a matriz abaixo, calcule a soma por linha e por coluna
mat1 <- matrix(c(1:50), nrow = 5, ncol = 5, byrow = T)
mat1

rowSums(mat1)    # Soma as linhas
colSums(mat1)    # Soma as colunas


# Exercício 9 - Para o vetor abaixo, ordene os valores em ordem crescente
a <- c(100, 10, 10000, 1000)
a

order(a)
a[order(a)]

# # Exercício 10 - Imprima no console todos os elementos da matriz abaixo que forem maiores que 15
mat1 <- matrix(c(1:50), nrow = 5, ncol = 5, byrow = T)
mat1

for(i in mat1){
  if(i >15){
    print(i)
  }
}
