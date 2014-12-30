require(forecast)
# Chargement des donees
data <- read.csv("~/Documents/MyProject/TimeSeries/data/France_1.csv", sep=";", dec=",",row.names = 1)

# on charge depuis la date 28/09/2007
#data <- data[554:1338,]
log.data <- log(data)

#allure des spreads
plot.ts(data$X1,xlab="annees",ylab="spreads",las=1)
plot.ts(log.data$X1,xlab="annees",ylab="spreads",las=1)
plot.ts(sqrt(data$X1),xlab="annees",ylab="spreads",las=1)
 
# Etude de la stationnarite -----------------------------------------------

install.packages('CADFtest')
# Test d'independance du Portemanteau pour les spreads
Box.test(x = data$X1, lag = 1)

# test de Dickey Fuller
require(tseries)
require(urca)
adf.test(x = data$X1,alternative = "s")
test2 <- ur.df(y = data$X1,type = "none")
summary(test2)
test3 <- ur.df(y = data$X1,type = "drift")
summary(test3)

# POUR LE PERIODIRAMME
require(TSA)

prd <- periodogram(y = log.data$X1[500:600],plot = TRUE)


# Independance ------------------------------------------------------------


