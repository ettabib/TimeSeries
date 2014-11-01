# Time Series Analysis
# TD2: Simulation

# Simulation of 1000 observations from a standard GWN
eps=rnorm(1000,mean=0,sd=1)
par(mfrow=c(1,1))
ts.plot(eps)
plot(density(eps))
qqnorm(eps)
par(mfrow=c(2,1))
acf(eps)
pacf(eps)
Box.test(eps,lag=1,type="Ljung")
eps2=eps*eps
par(mfrow=c(2,1))
acf(eps2)
pacf(eps2)
Box.test(eps2,lag=1,type="Ljung")

# Simulation of 1000 observations from a Student 3df GWN
epst=rt(1000,df=3)
par(mfrow=c(2,1))
ts.plot(epst)
plot(density(epst))
qqnorm(epst)
par(mfrow=c(2,1))
acf(epst)
pacf(epst)
Box.test(epst,lag=1,type="Ljung")

# Simulation of 1000 observations from a zero-mean Gaussian AR(1)
nobs=1000
phi1=1.0
eps=rnorm(nobs)
y=rep(0,nobs)
for(i in 2:nobs) y[i]=phi1*y[i-1]+eps[i]
par(mfrow=c(2,1))
ts.plot(y)
plot(density(y))
jarque.bera.test(y)
qqnorm(y)
par(mfrow=c(2,1))
acf(y)
pacf(y)
Box.test(y,lag=1,type="Ljung")

# phi1=0.8

# MA(1) Simulation
nobs=1000
theta1=0.9
eps=rnorm(nobs)
z=eps[2:1000]+theta1*eps[1:999]
length(z)
ts.plot(z)
plot(density(hist(z)))
qqnorm(z)
par(mfrow=c(2,1))
acf(z)
pacf(z)
Box.test(z,lag=10,type="Ljung")

# theta=0.8

