# Lista de Exercícios - Capítulo 12


# Configurando o diretório de trabalho
setwd("C:/Users/deeww/OneDrive/Área de Trabalho/Ciencia de dados/Curso Formação Cientista de dados/BigData_R_Azure_ML/cap12")
getwd()


# Existem diversos pacotes para árvores de decisão em R. Usaremos aqui o rpart.
install.packages('rpart')
library(rpart)

# Vamos utilizar um dataset que é disponibilizado junto com o pacote rpart
str(kyphosis)
head(kyphosis)
View(kyphosis)
?kyphosis

# Exercício 1 - Depois de explorar o dataset, crie um modelo de árvore de decisão
#esse é um modelo de classificação, porque as variáveis são categóricas
?rpart
arvore <- rpart(Kyphosis ~ . , method = 'class', data = kyphosis)
class(arvore)
arvore

# Para examinar o resultado de uma árvore de decisao, existem diversas funcões, mas você pode usar printcp()
printcp(arvore)

# Visualizando a ávore (execute uma função para o plot e outra para o texto no plot)
# Utilize o zoom para visualizar melhor o gráfico
plot(arvore, uniform = TRUE, main = "Arvore de Decisao em R")
text(arvore, use.n = TRUE, all = TRUE)

# Este outro pacote faz a visualização ficar mais legivel
install.packages('rpart.plot')
library(rpart.plot)
prp(arvore)


