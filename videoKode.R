# retrieve data
dftrans=read.csv("data/trans.csv", skip=1, sep = ";")


# clean data
# remove rows and cols
dftrans=dftrans[,-c(1,2)]
dftrans=dftrans[-1,]
dftrans=dftrans[-c(5:8),]
dftrans=dftrans[-4,]
rownames(dftrans)=NULL
cv <- colnames(dftrans)

# cleaning
cvclean <- gsub("X","",cv)
typeof(cvclean)
cvnum <- as.integer(cvclean)
typeof(cvnum)
ttrans=t(dftrans)
class(ttrans)
ttrans=as.data.frame(ttrans)
ttrans$year=cvnum
tnames=c("Ture","Km","Gods","Year")
colnames(ttrans)=tnames
rownames(ttrans)=NULL

# investigate
str(ttrans)
plot(ttrans$Year,ttrans$Ture, type="l", col="blue")
plot(ttrans$Year,ttrans$Gods, type="l", col="red")
plot(ttrans$Year,ttrans$Km, type="l", col="green")

# normalisering - min-max
# find min-max for alle tre transobser
# lav en funktion som kan normalisere hver observationstype
minture=min(ttrans$Ture)
maxture=max(ttrans$Ture)
mingods=min(ttrans$Gods)
maxgods=max(ttrans$Gods)
minkm=min(ttrans$Km)
maxkm=max(ttrans$Km)

# lav tre nye kolonner med de normaliserede værdier
ttrans$nture <- minmaxer(ttrans$Ture,minture,maxture)
ttrans$ngods <- minmaxer(ttrans$Gods,mingods,maxgods)
ttrans$nkm <- minmaxer(ttrans$Km,minkm,maxkm)

#plot samme graf
plot(ttrans$Year,ttrans$nture, type="l", col="blue")
lines(ttrans$Year,ttrans$ngods, type="l", col="red")
lines(ttrans$Year,ttrans$nkm, type="l", col="green")

minmaxer <- function(obs,mi,ma) {
  retval = (obs-mi)/(ma-mi)
  return(retval)
}





