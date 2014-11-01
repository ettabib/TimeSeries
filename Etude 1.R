
# Trafic mensuel de l’aéroport de Toulouse-Blagnac ------------------------

require(caschrono)
aa=read.table(file=system.file("/import/trafquoti.txt",package="caschrono"),
              header=FALSE,quote="",sep="",colClasses=c('numeric','character'),
              col.names=c('trafic','date'))

str(aa)
summary(aa)
date.1=as.Date(aa$date)
date.1[1:10]

#construction des dates de 1993 jusqua 2007
date.2=seq(from=as.Date("1993-01-01"),to=as.Date("2007-10-31"),by="day")
c(length(date.1),length(date.2))

# Agregation par annee, mois
an=substr(aa$date,1,4)
mois=substr(aa$date,6,7)
trafan=aggregate(aa$traf,list(An=an),sum)
str(trafan)
trafan=ts(trafan$x/1000,start=1993,frequency=1)
trafan.1=window(trafan,end=2006)

mois.an=as.numeric(paste(an,mois,sep=""))
trafmens=aggregate(aa$traf,list(Mois.An=mois.an),sum)
str(trafmens)
trafmensu=ts(trafmens$x/1000,start=c(1993,1),frequency=12)

# Décomposition de la série en tendance, saisonnalité et erreur -----------

dec.m=decompose(trafmensu)
plot(dec.m)
abline(v=2001.75)
