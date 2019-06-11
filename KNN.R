#Funcao KNN está no  pacote Class
library(class)

set.seed(1234)

#Lista de indices que servira para dividir o data frame com 1/3 dos dados para teste e 2/3 para treino
L = sample(1:nrow(iris), round(nrow(iris)/3))

#Separamos o data frame em dados de treino e de teste
teste = iris[L,1:4]
train = iris[-L,1:4]

#O algoritmo KNN aceito o parametro de classe a parte...
#a classe para o treinamento foi retirada do data frame com os mesmo indices do conjunto de treino
cla= factor(iris[-L, 5])

#os dados de treino, teste e classificaçao sao passados ao mesmo tempo para o algoritno
fir = knn(train, teste, cla, k = 3)

#a variavel fir é o resultado da classificaçao nos dados de teste
#para comparar o modelo com os dados reais, fazemos uma tabela de comparaçao e na funcáo cat...
#imprimimos a porcentagem de acerto do modelo para os dados reais
c_matrix = table(fir[1:length(L)], factor(iris[L,5]))
c_matrix
cat("Acerto:", sum(diag(c_matrix))/sum(c_matrix)*100, "%")



attr(fir ,"nn.index")
