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
  # a coluna 2 se refere ao numero de amigos belivers.
  pessoas<-matrix(nrow = NC, ncol = 2)
  
  for (i in 1:NC){
    pessoas[i,1]<-as.integer(rtriangle(1, qntMinAmigos,qntMax, (qntMinAmigos+qntMax)/2))
    pessoas[i,2]<-as.integer(rtriangle(1, qntMinAmigosB, pessoas[i,1], (qntMinAmigosB+pessoas[i,1])/2))
  }
  pessoas
}

calcula_infeccao<-function(NC = cenarios, pessoas){
  
  # matriz de chance de uma pessoa passar a ser um beiver
  # ou um not beliver.
  # coluna 1 -- chance de ser beliver.
  # coluna 2 -- chance de ser not beliver.
  prob<-matrix(nrow = NC, ncol = 2)
  
  for(i in 1:NC){
    beliver<-pessoas[i,2]
    Nbeliver<-pessoas[i,1]-pessoas[i,2]
    
    prob[i,1]<- beta( (beliver*(1-alfa)) / (( beliver*(1-alfa) +  Nbeliver*(1-alfa))) )
    prob[i,2]<- beta( (Nbeliver*(1-alfa)) / (( beliver*(1-alfa) +  Nbeliver*(1-alfa))) )
  }
  
  c(pessoas,prob)
}

pessoas<-gera_pessoas()
infec<-calcula_infeccao(cenarios,pessoas)