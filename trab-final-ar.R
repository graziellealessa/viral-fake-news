# Trabaho Final
# Modelagem de  Risco de Viralização Fake News
# Grupo: Beatriz de Andrade, Grazielle Alessa, Hugo Dantas e Jessica Genta

# Importanto bibliotecas
library(triangle)
library(igraph)

# Geração de dados iniciais
# Definição de número de amigos de uma pessoa
qntMinAmigos<-1
qntMax<-200
qntMinAmigosB<-0

# Parâmetros do modelo
beta<-0.7
alfa<-0.4

# Definicao do numero de cenarios
cenarios<-500

gera_pessoas<-function(NC = cenarios){
  
  # matriz de amigos da pessoa i.
  # para cada linha, temos:
  # a coluna 1 se refere ao numero total de amigos da pessoa
  # a coluna 2 se refere ao numero de amigos believers.
  pessoas<-matrix(nrow = NC, ncol = 2)
  
  for (i in 1:NC){
    pessoas[i,1]<-as.integer(rtriangle(1, qntMinAmigos,qntMax, (qntMinAmigos+qntMax)/2))
    pessoas[i,2]<-as.integer(rtriangle(1, qntMinAmigosB, pessoas[i,1], (qntMinAmigosB+pessoas[i,1])/2))
  }
  pessoas
}

calcula_infeccao<-function(NC = cenarios, pessoas){
  
  # matriz de chance de uma pessoa passar a ser um beiver
  # ou um not believer.
  # coluna 1 -- chance de ser believer.
  # coluna 2 -- chance de ser not believer.
  prob<-matrix(nrow = NC, ncol = 2)
  
  for(i in 1:NC){
    believer<-pessoas[i,2]
    Nbeliever<-pessoas[i,1]-pessoas[i,2]
    
    prob[i,1]<- beta( (believer*(1-alfa)) / (( believer*(1-alfa) +  Nbeliever*(1-alfa))) )
    prob[i,2]<- beta( (Nbeliever*(1-alfa)) / (( believer*(1-alfa) +  Nbeliever*(1-alfa))) )
  }
  
  c(pessoas,prob)
}

pessoas<-gera_pessoas()
infec<-calcula_infeccao(cenarios,pessoas)