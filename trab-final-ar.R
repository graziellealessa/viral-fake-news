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
bet<-0.3 # parametro de espalhamento
alf<-0.3 # parametro de credibilidade da noticia

# Definicao do numero de cenarios
cenarios<-3000

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

spreading_matrix<-function(NC = cenarios, pessoas, beta = bet, alfa = alf){
  
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


# gerando matriz de pessoas.
pessoas<-gera_pessoas()

# cenarios p beta = 0.3 e alfa variavel
pessoas_e_probabilidades1<-spreading_matrix(cenarios,pessoas,0.3,0.3)
pessoas_e_probabilidades2<-spreading_matrix(cenarios,pessoas,0.3,0.6)
pessoas_e_probabilidades3<-spreading_matrix(cenarios,pessoas,0.3,0.9)

# cumulativa das probabilidades beliver
#hist(pessoas_e_probabilidades[,3],main='Histograma das probabilidades - Beliver',xlab='Probabilidades',ylab='Frequência')
plot(ecdf(pessoas_e_probabilidades1[,3]),main='Função Cumulativa - Beliver',xlab='Chance',ylab='Probabilidades',col="blue")
par(new=T)
plot(ecdf(pessoas_e_probabilidades2[,3]),main='',xlab='',ylab='',axes=FALSE,col="green")
par(new=T)
plot(ecdf(pessoas_e_probabilidades3[,3]),main='',xlab='',ylab='',axes=FALSE,col="red")
legend("topleft", inset=.05, title="beta = 0.3", c("alfa = 0.3","alfa = 0.6","alfa = 0.9"), fill=c("blue", 'green', 'red'), horiz=FALSE)


# cumulativa das probabilidades not beliver
#hist(pessoas_e_probabilidades[,4],main='Histograma das probabilidades - Not Beliver',xlab='Probabilidades',ylab='Frequência')
plot(ecdf(pessoas_e_probabilidades1[,4]),main='Função Cumulativa - Not Beliver',xlab='Chance',ylab='Probabilidades',col="blue")
par(new=T)
plot(ecdf(pessoas_e_probabilidades2[,4]),main='',xlab='',ylab='',axes=FALSE,col="green")
par(new=T)
plot(ecdf(pessoas_e_probabilidades3[,4]),main='',xlab='',ylab='',axes=FALSE,col="red")
legend("bottomright", inset=.05, title="beta = 0.3", c("alfa = 0.3","alfa = 0.6","alfa = 0.9"), fill=c("blue", 'green', 'red'), horiz=FALSE)


# cenarios p beta = 0.6 e alfa variavel
pessoas_e_probabilidades1<-spreading_matrix(cenarios,pessoas,0.6,0.3)
pessoas_e_probabilidades2<-spreading_matrix(cenarios,pessoas,0.6,0.6)
pessoas_e_probabilidades3<-spreading_matrix(cenarios,pessoas,0.6,0.9)

# cumulativa das probabilidades beliver
#hist(pessoas_e_probabilidades[,3],main='Histograma das probabilidades - Beliver',xlab='Probabilidades',ylab='Frequência')
plot(ecdf(pessoas_e_probabilidades1[,3]),main='Função Cumulativa - Beliver',xlab='Chance',ylab='Probabilidades',col="blue")
par(new=T)
plot(ecdf(pessoas_e_probabilidades2[,3]),main='',xlab='',ylab='',axes=FALSE,col="green")
par(new=T)
plot(ecdf(pessoas_e_probabilidades3[,3]),main='',xlab='',ylab='',axes=FALSE,col="red")
legend("topleft", inset=.05, title="beta = 0.6", c("alfa = 0.3","alfa = 0.6","alfa = 0.9"), fill=c("blue", 'green', 'red'), horiz=FALSE)


# cumulativa das probabilidades not beliver
#hist(pessoas_e_probabilidades[,4],main='Histograma das probabilidades - Not Beliver',xlab='Probabilidades',ylab='Frequência')
plot(ecdf(pessoas_e_probabilidades1[,4]),main='Função Cumulativa - Not Beliver',xlab='Chance',ylab='Probabilidades',col="blue")
par(new=T)
plot(ecdf(pessoas_e_probabilidades2[,4]),main='',xlab='',ylab='',axes=FALSE,col="green")
par(new=T)
plot(ecdf(pessoas_e_probabilidades3[,4]),main='',xlab='',ylab='',axes=FALSE,col="red")
legend("bottomright", inset=.05, title="beta = 0.6", c("alfa = 0.3","alfa = 0.6","alfa = 0.9"), fill=c("blue", 'green', 'red'), horiz=FALSE)


# cenarios p beta = 0.6 e alfa variavel
pessoas_e_probabilidades1<-spreading_matrix(cenarios,pessoas,0.9,0.3)
pessoas_e_probabilidades2<-spreading_matrix(cenarios,pessoas,0.9,0.6)
pessoas_e_probabilidades3<-spreading_matrix(cenarios,pessoas,0.9,0.9)

# cumulativa das probabilidades beliver
#hist(pessoas_e_probabilidades[,3],main='Histograma das probabilidades - Beliver',xlab='Probabilidades',ylab='Frequência')
plot(ecdf(pessoas_e_probabilidades1[,3]),main='Função Cumulativa - Beliver',xlab='Chance',ylab='Probabilidades',col="blue")
par(new=T)
plot(ecdf(pessoas_e_probabilidades2[,3]),main='',xlab='',ylab='',axes=FALSE,col="green")
par(new=T)
plot(ecdf(pessoas_e_probabilidades3[,3]),main='',xlab='',ylab='',axes=FALSE,col="red")
legend("topleft", inset=.05, title="beta = 0.6", c("alfa = 0.9","alfa = 0.6","alfa = 0.9"), fill=c("blue", 'green', 'red'), horiz=FALSE)


# cumulativa das probabilidades not beliver
#hist(pessoas_e_probabilidades[,4],main='Histograma das probabilidades - Not Beliver',xlab='Probabilidades',ylab='Frequência')
plot(ecdf(pessoas_e_probabilidades1[,4]),main='Função Cumulativa - Not Beliver',xlab='Chance',ylab='Probabilidades',col="blue")
par(new=T)
plot(ecdf(pessoas_e_probabilidades2[,4]),main='',xlab='',ylab='',axes=FALSE,col="green")
par(new=T)
plot(ecdf(pessoas_e_probabilidades3[,4]),main='',xlab='',ylab='',axes=FALSE,col="red")
legend("bottomright", inset=.05, title="beta = 0.9", c("alfa = 0.3","alfa = 0.6","alfa = 0.9"), fill=c("blue", 'green', 'red'), horiz=FALSE)
