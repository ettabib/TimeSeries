# Time Series
# TD1: First Analysis

data()
EuStockMarkets
data.class(EuStockMarkets)
summary(EuStockMarkets)
plot(EuStockMarkets)
	
cac.ts=EuStockMarkets[,"CAC"]
plot(cac.ts)

# Differenciation
dcac40=diff(cac.ts)
rcac40=diff(log(cac.ts))*100
par(mfrow=c(2,1))
plot(dcac40)
plot(rcac40)

# 1) Underlying Unconditional Distribution function
# Numeric summaries
summary(rcac40)
kurtosis(rcac40)
# Graphical summaries
# Histogram
par(mfrow=c(2,2))
hist(rcac40,breaks=5)
hist(rcac40)
hist(rcac40,breaks=25)
hist(rcac40,breaks=50)
# Density
par(mfrow=c(1,1))
plot(density(rcac40))
x=seq(-5,5,0.1)
lines(x,dnorm(x,mean(rcac40),sd(rcac40)),lty=2,col="red")
# QQ plot
qqnorm(rcac40)
abline(0,1,col="red")

# Gaussian test
jarque.bera.test(rcac40)
ks.test(rcac40,pnorm)


# 2) Serial dependence
acf(rcac40)
pacf(rcac40)
Box.test(rcac40,lag=1,type="Box")
Box.test(rcac40,lag=10,type="Box")
Box.test(rcac40,lag=1,type="Ljung")

# Dependence on higher moments ?
rcac40.2=rcac40*rcac40
acf(rcac40.2)
pacf(rcac40.2)
Box.test(rcac40.2,lag=1,type="Box")


