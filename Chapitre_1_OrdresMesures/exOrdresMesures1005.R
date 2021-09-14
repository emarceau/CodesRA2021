# Cours A2021 : Act-3000
#
# Semestre : A2021
# 
# Chapitre : Mesures, Indices, Ordres stochastiques
#
# Exercice no5
#
# Loi de M : binomiale(nn,qq)
# Loi de N : Poisson(lambda)
# EM = EN = a
#
nn<-1000
a<-200
qq<-a/nn
lambda<-a
#
EM<-nn*qq
EN<-lambda
c(EM,EN)
#
VarM<-EM*(1-qq)
VarN<-EM
c(VarM,VarN)
#
kappa<-c(0.01,0.5,0.99)
VaRM<-qbinom(kappa,nn,qq)
VaRN<-qpois(kappa,lambda)
t(cbind(kappa,VaRM,VaRN))
#


