# Current Working Directory (WD)
getwd()
# Change of WD
setwd("D:/LF/Cours/MasterQEM")

# Charge packages from the toolbar : tseries and e1071

# Import Data
bnp.frame=read.delim2("bnp.txt")
data.class(bnp.frame)
bnp=bnp.frame[,1]
bnp.ts=ts(bnp)
plot(bnp.ts)

# Differencing
dbnp.ts=diff(log(bnp.ts))*100

# Graph
par(mfrow=c(2,1))
plot(bnp.ts)
plot(dbnp.ts)

# Data analysis and distribution
length(dbnp.ts)
summary(dbnp.ts)
moments(dbnp.ts)
mean(dbnp.ts)
sqrt(var(dbnp.ts))
skewness(dbnp.ts)
kurtosis(dbnp.ts)
par(mfrow=c(2,1))
hist(dbnp.ts,nclass=30)
plot(density(dbnp.ts))

par(mfrow=c(1,1))
plot(density(dbnp.ts),col="blue")
x=seq(min(dbnp.ts),max(dbnp.ts),0.1)
lines(x,dnorm(x,mean(dbnp.ts),sqrt(var(dbnp.ts))),lty=2,col="red")


jarque.bera.test(dbnp.ts)
qqnorm(dbnp.ts)

# Are moments constant over time ?
mean(window(dbnp.ts, start=1, end=500))
mean(window(dbnp.ts, start=1000, end=1500))
mean(window(dbnp.ts, start=2000, end=2500))
mean(window(dbnp.ts, start=2500, end=3000))

var(window(dbnp.ts, start=1, end=500))
var(window(dbnp.ts, start=1000, end=1500))
var(window(dbnp.ts, start=2000, end=2500))
var(window(dbnp.ts, start=2500, end=3000))

# F-test to be convinced
var.test(window(dbnp.ts, start=1, end=500),window(dbnp.ts, start=1000, end=1500))
var.test(window(dbnp.ts, start=2501, end=3000),window(dbnp.ts, start=1000, end=1500))



# ACF 
acf(dbnp.ts)
pacf(dbnp.ts)
Box.test(dbnp.ts,lag=1)
Box.test(dbnp.ts,lag=2)

# Specification
ar.mod=ar(dbnp.ts,AIC=T)
par(mfrow=c(1,1))
plot(ar.mod$aic,type="l")

# We choose a ARMA(1,2)
# Estimation
arma(dbnp.ts,order=c(1,2),include.intercept=F)
arima(dbnp.ts,order=c(1,0,2),include.mean=F)
dbnp.mod=arima(dbnp.ts,order=c(1,0,2),include.mean=F)

# Validation
dbnp.mod
tsdiag(dbnp.mod)
res=dbnp.mod$residuals
resstd=res/sd(res)
plot(res)
plot(resstd)
Box.test(res,lag=1)
Box.test(res,lag=2)
jarque.bera.test(res)
qqnorm(res)

# Forecasting
horiz=20
predict(dbnp.mod,n.ahead=horiz)
dbnp.prev=predict(dbnp.mod,n.ahead=horiz)$pred
par(mfrow=c(1,1))
ts.plot(window(dbnp.ts,start=3000,end=3126),dbnp.prev)
dbnp.se=predict(dbnp.mod,n.ahead=horiz)$se
bornesup=dbnp.prev+1.96*dbnp.se
borneinf=dbnp.prev-1.96*dbnp.se
ts.plot(bornesup,borneinf,dbnp.prev,window(dbnp.ts,start=3000,end=3126))


# In-sample dynamic forecasting for h=1
dbnp.prev.dyn=ts(start=3101,end=3126)
dbnp.se.dyn=ts(start=3101,end=3126)

for (i in 3101:3126){
dbnp.mod.temp=arima0(window(dbnp.ts,start=1,end=i),order=c(1,0,2),include.mean=F)
dbnp.prev.dyn[i-3100]=predict(dbnp.mod.temp,n.ahead=1)$pred
dbnp.se.dyn[i-3100]=predict(dbnp.mod.temp,n.ahead=1)$se
}

par(mfrow=c(1,1))
plot(window(dbnp.ts,start=3000,end=3126),col="orange")
lines(dbnp.prev.dyn,col="red")
lines(dbnp.prev.dyn+1.00*dbnp.se.dyn,col="blue")
lines(dbnp.prev.dyn-1.00*dbnp.se.dyn,col="blue")

# RMSE
rmse=sqrt(mean((dbnp.ts-dbnp.prev.dyn)**2))
rmse

# Comments

# GARCH process
plot(res)
res2=res*res
acf(res2)
pacf(res2)
Box.test(res2)

# AR process on squared residuals
ar(res2,aic=T)
res2.mod=arima(res2,order=c(1,0,1))

dbnp.garch=garch(res,order=c(1,1))
summary(dbnp.garch)
plot(dbnp.garch)
nu=dbnp.garch$residuals
par(mfrow=c(2,1))
plot(res)
plot(nu)
var(res)
var(nu,na.rm=T)
plot(fitted(dbnp.garch))


predict(dbnp.garch,n.ahead=1,genuine=T)[,1][3127]

par(mfrow=c(1,1))
plot(window(dbnp.ts,start=3000,end=3126),col="orange")
lines(dbnp.prev.dyn,col="red")
lines(dbnp.prev.dyn+1.00*window(fitted(dbnp.garch),start=3101,end=3126)[,1],col="blue")
lines(dbnp.prev.dyn-1.00*window(fitted(dbnp.garch),start=3101,end=3126)[,1],col="blue")
