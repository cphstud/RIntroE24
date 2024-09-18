library(readxl)

trans <- read.csv("data/trans.csv", skip = 1, sep = ";")
trans = trans[-c(1,5:8),3:length(trans)]
#trans = trans[-1,]
#trans = trans[,-c(1,2)]
#firstrow=trans[1,]

transnavne=colnames(trans)
transnavne

teststr=transnavne[1]
nyenavne=as.numeric(gsub("X","",transnavne))

ttrans=as.data.frame(t(trans))

oknames=c("Antal_Ture","Kørte_Km","Gods")
colnames(ttrans)=oknames
rownames(ttrans)=NULL

plot(ttrans$Gods, type="b")
#lines(ttrans$Gods, col="red")


#find min og max
maxkm=max(ttrans$Gods)
minkm=min(ttrans$Gods)

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


# lav en funktion
testfunktion  <- function(number, faktor) {
  tempval = number*faktor
  return(tempval)
}


xnorm <- function(x,mi,ma) {
  tmp=(x-mi)/(ma-mi)
  return(tmp)
}

plot(transt$year,transt$Ture, type="b", col="blue", frame = FALSE )
plot(ttrans$year,ttrans$NTure, type="b", col="blue", frame = FALSE )
lines(ttrans$year,ttrans$NKm,type="b",col="red")
lines(ttrans$year,ttrans$NGods,type="b",col="green")

# ggplot
ggplot(ttrans, aes(x=year)) +
  geom_line(aes(y=NTure, color="NTure"))+
  geom_line(aes(y=NKm, color="NKm"))+
  geom_line(aes(y=NGods, color="NGods"))


ggplot(ttrans, aes(x=year, y=NGods)) +
  #geom_bar(stat="identity")+
  geom_col()+
  geom_point()+
  geom_line()


head(plottrans)

# tidy format
library(tidyr)
library(dplyr)
colnames(ttrans)
plottrans=ttrans %>% select(year,NTure,NKm,NGods)
plottrans$NTure=round(plottrans$NTure*1000,1)
plottrans$NKm=round(plottrans$NKm*1000,1)
plottrans$NGods=round(plottrans$NGods*1000,1)

data_long <- pivot_longer(
  plottrans,
  cols = c(NTure, NKm, NGods),    # Columns to pivot
  names_to = "Category",          # New column to hold the names
  values_to = "Value"             # New column to hold the values
)



# reshape: tar observations-kolonnernes indhold (varierende tal) og samler dem i en ny kolonne
# Den nye kolonne hedder så "Value". Kolonnernes overskrift (tallenes kategori)
# flyttes over og bliver til en variende character i én kolonne som så hedder "Category"
dfw <- data_long %>% pivot_wider(names_from = Category, values_from = Value)

ggplot(r, aes(year,y=Value, color=Cat))+
  geom_line()

library(tidyr)
library(corrplot)
ntrcc <- ntrc %>% mutate(across(everything(), as.numeric))

# normaliser med min-max
correlation_matrix <- cor(ntrcc,use = "complete.obs")
corrplot(correlation_matrix, type = "upper")


ttransp=ttrans[,1:4]
