require(caschrono)
require(fSeries)
data(m30)
plot.ts(m30,xlab="année",ylab="nombre de morts",las=1)
polygon(c(1995,2005,2005,1995),c(340,340,910,910),lty=2)
deb=c(1995,1); fin=c(2004,12) # zoom
plot.ts(window(m30,start=deb,end=fin),xlab="année",ylab="nombre de morts")

# Analyse Spectrale 
require(TSA)
periodogram(nottem)
#aa=periodogram(nottem,plot=FALSE)
aa=periodogram(France_1$X1,plot=FALSE)
# Frequence du pique
aa$freq[which.max(as.vector(aa$spec))]
#Cherchons les 5 fréquences de plus grand périodogramme
ab=order(-aa$spec)[1:5]
frq.spe=rbind(aa$freq[ab],aa$spec[ab])
rownames(frq.spe)= c("F́equence","P ́eriodogramme")
frq.spe


#generation processus periodique
nData <- 1000
#frequence
freq <- 
data <- cos(1:nData) + rnorm(n = nData,mean = 0,sd = 1)
plot.ts(data)
# introducing dependencies
data <- c(data[1:nData-1],0) + c(0,data[2:nData])
plot.ts(data)

aa=periodogram(data)
