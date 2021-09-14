# Cours A2021 : Act-3000
#
# Semestre : A2021
# 
# Chapitre : Mesures, Indices, Ordres stochastiques
#
# Exercice no3
#
# Loi de X1 : lognormale(mu1,sigma1)
# Loi de X2 : lognormale(mu2,sigma2)
# EX1 = EX2 = a
#
sigma1 <-0.5
sigma2 <-1
a<-10 
#
EX1<-a
EX2<-a
#
VarX1<-(EX1^2)*(exp(sigma1^2)-1) 
VarX2<-(EX2^2)*(exp(sigma2^2)-1)
c(VarX1,VarX2)
#
phiMoins1KappaC<-(sigma1+sigma2)/2
kappaC<-pnorm(phiMoins1KappaC)
C<-a*exp(0.5*(sigma1 * sigma2))
c(phiMoins1KappaC,kappaC,C)
#
#
mu1<-log(EX1)-0.5*(sigma1^2)
mu2<-log(EX2)-0.5*(sigma2^2)
c(mu1,mu2)
#
plnorm(C,mu1,sigma1)
plnorm(C,mu2,sigma2)
#
kappa<-c(0.01,0.5,0.99,kappaC)
VaRX1<-qlnorm(kappa,mu1,sigma1)
VaRX2<-qlnorm(kappa,mu2,sigma2)
t(cbind(kappa,VaRX1,VaRX2))
#
TVaRX1<-1/(1-kappa)*a*(1-pnorm(qnorm(kappa)-sigma1))
TVaRX2<-1/(1-kappa)*a*(1-pnorm(qnorm(kappa)-sigma2))
t(cbind(kappa,TVaRX1,TVaRX2))
#
