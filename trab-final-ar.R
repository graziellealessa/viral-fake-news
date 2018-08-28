# Trabaho Final
# Modelagem de fake news
# Grupo: Beatriz de Andrade, Grazielle Alessa, Hugo Dantas e Jessica Genta

# Importanto bibliotecas
library(triangle)

# Geracao de dados iniciais
# Definicao de num. de amigos de uma pessoa
qntMinAmigos<-1
qntMax<-500
qntMinAmigosB<-0

# Parametros do modelo
beta<-0.7 # parametro de espalhamento
alfa<-0.4 # parametro de credibilidade da noticia

# Definicao do numero de cenarios
cenarios<-500

gera_pessoas<-function(NC = cenarios){
  
  # matriz de amigos da pessoa i.
  # para cada linha, temos:
  # a coluna 1 se refere ao numero total de amigos da pessoa
  # a coluna 2 se refere ao numero de amigos belivers.
  pessoas<-matrix(nrow = NC, ncol = 2)
  
  for (i in 1:NC){
    pessoas[i,1]<-as.integer(rtriangle(1, qntMinAmigos,qntMax, (qntMinAmigos+qntMax)/2))
    pessoas[i,2]<-as.integer(rtriangle(1, qntMinAmigosB, pessoas[i,1], (qntMinAmigosB+pessoas[i,1])/2))
  }
  pessoas
}

spreading_matrix<-function(NC = cenarios, pessoas){
  
  # matriz de chance de uma pessoa passar a ser um believer
  # ou um not beliver.
  # coluna 1 -- fi
  # coluna 2 -- gi
  spreading_functions_matrix<-matrix(nrow = NC, ncol = 2)
  
  for(i in 1:NC){
    nbi<-pessoas[i,2]
    nfi<-pessoas[i,1]-pessoas[i,2]
    
    spreading_functions_matrix[i,1]<- beta*( (nbi*(1+alfa)) / (( nbi*(1+alfa) +  nfi*(1-alfa))) )
    spreading_functions_matrix[i,2]<- beta*( (nfi*(1-alfa)) / (( nbi*(1+alfa) +  nfi*(1-alfa))) )
  }
  cbind(pessoas,spreading_functions_matrix)
}

pessoas<-gera_pessoas()
pessoas_e_probabilidades<-spreading_matrix(cenarios,pessoas)

#histograma e cumulativa das probabilidades fi
hist(pessoas_e_probabilidades[,3],main='Histograma das probabilidades - Beliver',xlab='Probabilidades',ylab='Frequência')
plot(ecdf(pessoas_e_probabilidades[,3]),main='Função Cumulativa - Beliver',xlab='Chance',ylab='Probabilidades')

#histograma e cumulativa das probabilidades gi
hist(pessoas_e_probabilidades[,4],main='Histograma das probabilidades - Not Beliver',xlab='Probabilidades',ylab='Frequência')
plot(ecdf(pessoas_e_probabilidades[,4]),main='Função Cumulativa - Not Beliver',xlab='Chance',ylab='Probabilidades')
