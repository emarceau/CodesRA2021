# Cours A2021 : Act-3000
#
# Semestre : A2021
# 
# Chapitre : Mesures, Indices, Ordres stochastiques
#
# Exercice no5
#
#
# Illustration - Portefeuille de n v.a. iid
#
# S=X1+...+Xn 
# Xi se comporte comme X
# 
# Loi de X : Gamma
aa<-0.5
bb<-aa/10
#
n1<-1
n2<-10
n3<-100
n4<-1000
#
vx<-(0:400)/10
FW1<-pgamma(vx,aa*n1,bb*n1)
FW2<-pgamma(vx,aa*n2,bb*n2)
FW3<-pgamma(vx,aa*n3,bb*n3)
FW4<-pgamma(vx,aa*n4,bb*n4)
matplot(vx,cbind(FW1,FW2,FW3,FW4),type="l",xlab="x",ylab="F_{Wn}(x)")
#
vx<-(0:400)/10
fW1<-dgamma(vx,aa*n1,bb*n1)
fW2<-dgamma(vx,aa*n2,bb*n2)
fW3<-dgamma(vx,aa*n3,bb*n3)
FW4<-dgamma(vx,aa*n4,bb*n4)
matplot(vx,cbind(fW1,fW2,fW3,FW4),type="l",xlab="x",ylab="f_{Wn}(x)")
#
EX<-10
vkappa<-(1:9999)/10000
VaRW1<-qgamma(vkappa,aa*n1,bb*n1)
VaRW2<-qgamma(vkappa,aa*n2,bb*n2)
VaRW3<-qgamma(vkappa,aa*n3,bb*n3)
VaRW4<-qgamma(vkappa,aa*n4,bb*n4)
TVaRW1<-EX*(1-pgamma(VaRW1,aa*n1+1,bb*n1))/(1-vkappa)
TVaRW2<-EX*(1-pgamma(VaRW2,aa*n2+1,bb*n2))/(1-vkappa)
TVaRW3<-EX*(1-pgamma(VaRW3,aa*n3+1,bb*n3))/(1-vkappa)
TVaRW4<-EX*(1-pgamma(VaRW4,aa*n4+1,bb*n4))/(1-vkappa)
matplot(vkappa,cbind(TVaRW1,TVaRW2,TVaRW3,TVaRW4),type="l",xlab="kappa",ylab="TVaR_{kappa}(Wn)")
#
matplot(vkappa,cbind(TVaRW3,TVaRW4),type="l",xlab="kappa",ylab="TVaR_{kappa}(Wn)")
#