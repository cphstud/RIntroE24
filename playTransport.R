trans <- read.csv("data/trans.csv", skip = 1, sep = ";")
trans = trans[-c(1,5:8),3:length(trans)]
transnavne=colnames(trans)
transnavne

teststr=transnavne[1]
nyenavne=as.numeric(gsub("X","",transnavne))

# Transpose
ttrans=as.data.frame(t(trans))

oknames=c("Antal_Ture","Kørte_Km","Gods")
colnames(ttrans)=oknames
rownames(ttrans)=NULL

plot(ttrans$Gods, type="b")
#lines(ttrans$Gods, col="red")


ttrans$year=nyenavne
colnames(ttrans)=c("ture","km","gods","year")

# beregn min og max for hver type observation
mit=min(ttrans$ture)
mat=max(ttrans$ture)
mik=min(ttrans$km)
mak=max(ttrans$km)
mig=min(ttrans$gods)
mag=max(ttrans$gods)

ttrans$NTure=xnorm(ttrans$ture,mit,mat)
ttrans$NTure=xnorm(ttrans$ture, mit,mat)
ttrans$NKm=xnorm(ttrans$km, mik,mak)
ttrans$NGods=xnorm(ttrans$gods, mig,mag)


xnorm <- function(x,mi,ma) {
  tmp=(x-mi)/(ma-mi)
  return(tmp)
}

# base R graphics
plot(ttrans$year,ttrans$NTure, type="b", col="blue", frame = FALSE )
lines(ttrans$year,ttrans$NKm,type="b",col="red")
lines(ttrans$year,ttrans$NGods,type="b",col="green")

# ggplot
ggplot(ttrans, aes(x=year)) +
  geom_line(aes(y=NTure, color="NTure"))+
  geom_line(aes(y=NKm, color="NKm"))+
  geom_line(aes(y=NGods, color="NGods"))



plottrans=ttrans[,c("year","NTure","NKm","NGods")]
# tidy format
library(tidyr)
library(dplyr)
plottrans=ttrans %>% select(year,NTure,NKm,NGods)
head(plottrans)

data_long <- pivot_longer(
  plottrans,
  cols = c(NTure, NKm, NGods),    # Columns to pivot
  names_to = "Category",          # New column to hold the names
  values_to = "Value"             # New column to hold the values
)


ggplot(data_long, aes(year,y=Value, color=Category))+
  geom_line()

  