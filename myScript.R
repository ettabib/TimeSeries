# Chargement des donees
France_1 <- read.csv("~/Documents/MyProject/TimeSeries/data/France_1.cvs.csv", sep=";", dec=",",row.names = 1)
Spreads <- France_1[1:10]

#allure des spreads
plot.ts(France_1$X1,xlab="annees",ylab="spreads",las=1)

#Test de Portemanteau
require(caschrono)
set.seed(123)
y1=arima.sim(n=100,list(ar=-.7),sd=sqrt(4))
y2=arima.sim(n=100,list(ar=c(rep(0,11),-.7)),sd=sqrtt(4))
ret=c(3,6,9,12)
a1=Box.test.2(y1,nlag=ret,type="Ljung-Box",decim=2)
a2=Box.test.2(y2,nlag=ret,type="Ljung-Box",decim=2)
a12=cbind(a1,a2[,2])
colnames(a12)=c("Retard","p-val. y1","p-val.y2")
a12

# Test d'independance du Portemanteau pour les spreads
Box.test(x = France_1$X1, lag = 1)
# -> distance qui-2 tres grande
