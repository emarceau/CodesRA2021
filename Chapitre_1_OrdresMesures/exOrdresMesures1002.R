# Cours A2021 : Act-3000
#
# Semestre : A2021
# 
# Chapitre : Mesures, Indices, Ordres stochastiques
#
# Exercice no2
#
beta1<-1/2
beta2<-1/3
beta3<-1/5
c1<-(beta2/(beta2-beta1))*(beta3/(beta3-beta1))
c2<-(beta1/(beta1-beta2))*(beta3/(beta3-beta2))
c3<-(beta1/(beta1-beta3))*(beta2/(beta2-beta3))
c(c1,c2,c3)
sum(c1,c2,c3)
#
kappa<-0.999
VaRS3<-qgamma(kappa,3,beta1)
VaRT3<-qgamma(kappa,3,beta3)
c(VaRS3,VaRT3)
#
pErlangGeneralisee<-function(x) c1*pexp(x,beta1)+c2*pexp(x,beta2)+c3*pexp(x,beta3)
pErlangGeneralisee(10)
f<-function(x) abs(pErlangGeneralisee(x)-kappa)
res<-optimize(f,c(VaRS3,VaRT3))
res
VaRR3<-res$minimum
#
c(VaRS3,VaRR3,VaRT3)
