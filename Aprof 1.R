library(MASS)
library(class)
library(tidyverse)

?biopsy
str(biopsy)

#-------------------------Tratamento dos dados ---------------------------

dados_originais <- biopsy
#retira os NA da coluna V6
dados_tratados <- filter(dados_originais, !is.na(V6))
#A coluna ID não e necessario para a analise, foi retirada na linha abaixo
dados_tratados <- dados_tratados[,-1]

str(dados_tratados)

#-----------------Coleta de amostras e treino do modelo-------------------------------


porcentagens <- data.frame()

for (i in 1:100)
{
      #dados de indice e amostras de treino, teste e classe
      idx <- sample(1:nrow(dados_tratados), round(nrow(dados_tratados)/3))
      teste <- dados_tratados[idx, 1:9]
      treino <- dados_tratados[-idx, 1:9]
      classe <- dados_tratados[-idx, 10]
      
      #k = 12 
      fit <- knn(treino, teste, classe, k = 12)
      
      #tabela conferindo o resultado do teste KNN com os dados reais
      conf <- table(fit[1:length(idx)], dados_tratados[idx,10])

      cat("Acerto:", sum(diag(conf))/sum(conf)*100, "% \n")
      
      #Porcentagem registrada em um dataframe
      porcentagens[i,1] <- sum(diag(conf))/sum(conf)*100 
}

#média dos 100 resultados
mean(porcentagens[1,1])
