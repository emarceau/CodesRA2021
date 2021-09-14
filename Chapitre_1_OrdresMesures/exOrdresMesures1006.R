# Semestre : A2021
# 
# Chapitre : Mesures, Indices, Ordres stochastiques
#
# Exercice no5
#
# Loi de M : Poisson(lambda)
# Loi de M' : Poisson(lambdaPrime)
#
lambda<-2
lambdaPrime<-5
kappa<-c(0.01,0.5,0.99)
VaRM<-qpois(kappa,lambda)
VaRMPrime<-qpois(kappa,lambdaPrime)
t(cbind(kappa,VaRM,VaRMPrime))
