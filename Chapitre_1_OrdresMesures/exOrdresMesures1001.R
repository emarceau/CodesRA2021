# Cours Act-3000
# 
# Semestre A2021
# 
# Chapitre : Mesures, Indices, Ordres stochastiques
#
# Exercice no1
#
# Distribution : Erlang Généralisée
beta1<-1/10
beta2<-1/20
coef1<-beta2/(beta2-beta1)
coef2<-1-coef1
pErlangGeneralisee<-function(x) 1-coef1*exp(-beta1*x)-coef2*exp(-beta2*x)
kap<-0.99
pErlangGeneralisee(100)
#
# Calcul de la VaR avec optimize
f<-function(x) abs(pErlangGeneralisee(x)-kap)
res<-optimize(f,c(100,1000))
VaR<-res$minimum
c(VaR,pErlangGeneralisee(VaR))
#
#
# Distribution : gamma
#
qgamma(kap,2,1/10)
beta3<-2/((1/beta1)+(1/beta2))
beta3
qgamma(kap,2,beta3)
#
# 