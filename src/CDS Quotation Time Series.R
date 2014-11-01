
# CDS Quotation Time series -----------------------------------------------

# Preparation des donees
aa=read.csv("~/Documents/MyProject/TimeSeries/data/France_1.csv", sep=";", dec=",")

str(aa)
summary(aa)
date.1=as.Date(aa$Dates.Cotations)
date.1[1:10]

#construction des dates de 1993 jusqua 2007
date.2=seq(from=as.Date("2005-08-16"),to=as.Date("2010-09-30"),by="day")
c(length(date.1),length(date.2))

# Agregation par annee, mois
an=substr(aa$Dates.Cotations,1,4)
mois=substr(aa$Dates.Cotations,6,7)
volan=aggregate(aa$X1,list(An=an),sd)
str(volan)
volan=ts(volan$x/1000,start=2005,frequency=1)
volan.1=window(volan,end=2010)

mois.an=as.numeric(paste(an,mois,sep=""))
volmens=aggregate(aa$X1,list(Mois.An=mois.an),sd)
str(volmens)
volmensu=ts(volmens$x/1000,start=c(2005,1),frequency=12)

# Décomposition de la série en tendance, saisonnalité et erreur -----------

dec.m=decompose(volmensu)
plot(dec.m)
abline(v=2008)

# Month plot --------------------------------------------------------------

monthplot(volmensu)

