# Packages ----------------------------------------------------------------
library("Hmisc")
require(caschrono)
require(het.test)
require(bstats)
library(ggplot2)  #pour les graphiques
library(data.table)
library(reshape2)  #pour la fonction melt
library(scales)
library(ggplot2)
library(forecast)
library(tseries)
require(FitAR)
require(forecast)
require(tseries)
require(urca)
require(TSA)
require(CADFtest)
library(Kendall)
library(descomponer)
require(FinTS)
require(fGarch)

# Chargement donne --------------------------------------------------------

data <- read.csv("~/Documents/MyProject/TimeSeries/data/France_1.csv", sep=";", dec=",")
# 28/09/2007 date a partir de laquelle le marche devient complet
# on charge depuis la date 28/09/2007 - 554
debut <- 1
fin <- 700
data <- data[debut:fin,]
comment(data) <- "Spreads de la societe AIG"
str(data)
head(data)
data$Dates.Cotations <- as.Date(data$Dates.Cotations)
colnames(data) <- c("cot","S1","S2","S3","S4","S5","S6","S7","S8","S9","S10")
debut
data$S1 <- log(data$S1)
S1 <- data$S1
Cot <- data$cot


# Exploration des donnees -------------------------------------------------
head(S1)
boxplot(x = data$S1,data=data, id.method = "y",varwidth = TRUE,
        names = c("Spreads"),boxwex = 0.5,border = "blue",
        col = "grey",horizontal = TRUE,ylab="Spreads",main = "Boxplot des log.spreads")
describe(S1)
ggplot(data, aes(x = cot, y = S1)) + geom_line(col="orange") + geom_point(col="red") + geom_hline(aes(yintercept = mean(S1)), color = "blue")


# Agregation annuelle -----------------------------------------------------

data <- as.data.table(data)
class(data)
testy <- data[, mean(S1), by = year(cot)]
head(testy)
setnames(testy, "V1", "meanSpreads")
ggplot(testy, aes(x = year, y = meanSpreads)) + geom_line() + geom_point()
ggplot(data, aes(x = factor(year(Cot)), y = S1)) + geom_boxplot()



# Spreads moyennes mensuelles  ---------------------------------------------

data <- as.data.table(data)
testm <- data[, mean(S1), by = month(Cot)]
head(testm)
setnames(testm, "V1", "Spreads")
testm$mois = seq(as.Date("2008-01-01"), by = "month", length = 12)

# Spreads moyens mensuelles :
ggplot(testm, aes(x = mois, y = Spreads)) + geom_line() + geom_point() + scale_x_date(labels = date_format("%B"), 
                                                                                    breaks = date_breaks("month"))
# Boxplot des spreads par mois :
ggplot(data, aes(x = factor(month(Cot)), y = S1)) + geom_boxplot()



# Superposition des ventes par année  -------------------------------------
library(data.table)
data$annee = year(data$cot)
data2 <- as.data.table(data)
sup <- data2[annee %between% c(2005, 2010)]
sup$label <- as.factor(paste("Spreads", sup$annee, sep = ""))
sup$month <- seq(as.Date("2005-08-01"), by = "month", length = 12)
ggplot(data = sup, aes(x = month, y = S1, color = label)) + geom_line(aes(y = S1)) +  geom_point(aes(y = S1)) + scale_colour_manual(name = "Variables", values = c(Spreads2005="purple",Spreads2006="pink",Spreads2007 = "blue", Spreads2008 = "red",Spreads2009 ="green",Spreads2010="yellow")) + scale_x_date(labels = date_format("%B"), breaks = date_breaks("month")) + theme(legend.position ="bottom")
# plot en coordonnees polaires
ggplot(data = sup, aes(x = month, y = S1, color = label)) + geom_line(aes(y = S1)) +   geom_point(aes(y = S1)) + coord_polar() + scale_x_date(labels = date_format("%B"), breaks = date_breaks("month")) 

# Stationnarite -----------------------------------------------------------

data$cot <- seq(as.Date("2008-08-16"), by = "month", along = data$S1)
data.ts <- ts(data$S1, start = 2000, freq = 12)
library("CADFtest")
# test de non stationnarite avec tendance linéaire
adft <- CADFtest(data.ts, max.lag.y = 11, type = "trend", criterion = "BIC")
summary(adft)
adft$p.value

# Etude de la stationnarite -----------------------------------------------

# Test d'independance du Portemanteau pour les spreads
Box.test(x = S1)
# test de Dickey Fuller
adf.test(x = S1,alternative = "s")
test2 <- ur.df(y = S1,type = "none")
summary(test2)
test3 <- ur.df(y = data$X1,type = "drift")
summary(test3)

# Elimination de la tendance par differenciation ----------------------------------------------
D1 <- diff(x = S1,differences = 1)
D2 <- diff(x = S1,differences = 2)
plot.ts(D1,type="l")
plot.ts(D2,type="l")
# test d'independance
Box.test(x = D1)
Box.test(x = D1,type = "L") 
adf.test(x = D1,alternative = "s") # -> processus stationnaire
summary(CADFtest(D1,type = "none",criterion = "BIC",max.lag.y = 11))
summary(ur.kpss(D1,type ="mu"))
summary(ur.kpss(D1,type ="tau"))

Box.test(x = D2)
Box.test(x = D2,type = "L") 
adf.test(x = D2,alternative = "s") # -> processus stationnaire
summary(CADFtest(D2,type = "trend",criterion = "BIC",max.lag.y = 1))
summary(ur.kpss(D2,type ="mu"))
summary(ur.kpss(D2,type ="tau"))


# la tendance a de forte chane d'etre lineaire mt = a*t+b
lev <- 5
best.ar <- auto.arima(x = S1,d = 1,max.p = lev,max.q = lev,start.p = 0,start.q = 0,allowdrift = TRUE,trace = TRUE,ic = "aic",seasonal = FALSE)
best.ar <- auto.arima(x = S1,d = 2,max.p = lev,max.q = lev,start.p = 0,start.q = 0,allowdrift = TRUE,trace = TRUE,ic = "aic",seasonal = FALSE)

# Choix du modele ---------------------------------------------------------

plot()
ac <- acf(x = D2,lag.max = 30)
summary(ac)
M0 <- arima(S1,c(8,1,4))
M1 <- arima(S1,c(7,1,5))
M2 <- arima(S1,c(6,1,2))
M3 <- arima(S1,c(3,1,5))

M0$aic
M1$aic
M2$aic
M3$aic


# Comparaison des stationnarite des residus

Box.test(M0$residuals)$p.value
Box.test(M1$residuals)$p.value
Box.test(M2$residuals)$p.value
Box.test(M3$residuals)$p.value

adf.test(x = M0$residuals,alternative = "s")
adf.test(x = M1$residuals,alternative = "s")
adf.test(x = M2$residuals,alternative = "s")
adf.test(x = M3$residuals,alternative = "s")

#Est ce que les residus suivent la loi normale
shapiro.test(M0$residuals)$p.value
shapiro.test(M1$residuals)$p.value
shapiro.test(M2$residuals)$p.value
shapiro.test(M3$residuals)$p.value

graphe.proba.norm(M0$residuals)
graphe.proba.norm(M1$residuals)
graphe.proba.norm(M2$residuals)
graphe.proba.norm(M3$residuals)


# Performance -------------------------------------------------------------

par(mfrow = c(2, 1))
diffGNP <- S1
T <- length(S1)
n <- 10
annee <- Cot
index <- 1:(fin - n - 1)
res0 <- predict(arima(S1[index], c(1, 1, 0)), n)
res1 <- predict(arima(S1[index], c(2, 1, 2)), n)
plot(Cot[(T - 4 * n):T], diffGNP[(T - 4 * n):T - 1], main = "prevision ARMA(0,13)", t = "l", col = "blue", xlab = "temps", ylab = "S1")
lines(annee[(T - n):T], c(diffGNP[T - n - 1], res0$pred),col = "red")
lines(annee[(T - n):T], c(diffGNP[T - n - 1], res0$pred) + c(0,res0$se) * 1.96, lty = 2)
lines(annee[(T - n):T], c(diffGNP[T - n - 1], res0$pred) - c(0,res0$se) * 1.96, lty = 2)
plot(Cot[(T - 4 * n):T], diffGNP[(T - 4 * n):T - 1], main = "prevision ARMA(4,3)", t = "l", col = "blue", xlab = "temps", ylab = "S1")
lines(annee[(T - n):T], c(diffGNP[T - n - 1], res1$pred),col = "green")
lines(annee[(T - n):T], c(diffGNP[T - n - 1], res1$pred) + c(0,res1$se) * 1.96, lty = 2)
lines(annee[(T - n):T], c(diffGNP[T - n - 1], res1$pred) - c(0,res1$se) * 1.96, lty = 2)

