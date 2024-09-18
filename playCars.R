library(httr2)
library(skimr)
mcard=read.csv("data/bilbasen.csv")
head(mcard)


# stats on raw data
res=skim(mcard)

# cleaning
missc=mcard[is.na(mcard$mpg),]
clcars=mcard[!is.na(mcard$mpg),]
clcars=clcars[,-c(1,5,9,12,13)]
head(clcars)
colnames(clcars)
#feature-engineering - year to age
clcars$age=2024-clcars$year


# PLAY ML on price
plot(clcars$age, clcars$price)
#ym=lm(data=clcars, price ~ poly(age,3))
ym=lm(data=clcars, price ~ poly(age,1))
rr=ym$coefficients
rr

ggplot(clcars, aes(x=age,y=price))+
  geom_point()+
  geom_smooth(method = "lm", formula = y ~ poly(x, 3), se = FALSE)

#extrapolating - all numbers with comma for displacement
#1) For one
teststr="Ford Ka 1,2 Trend 3d"
teststr="Volvo XC40 2,0 T4 190 Momentum aut. 5d"
testdf=clcars[1:5,]
mp="[0-9]+,[0-9]+"
mp="\\d+,\\d+"
mr=regexpr(pattern = mp, teststr)
regmatches(teststr,mr)

#2) For a few
mrm=regexpr(pattern = mp, testdf$maketype)
displ=regmatches(testdf$maketype, mrm)
displ

#3) For all
mra=regexpr(pattern = mp, clcars$maketype)
displ=regmatches(clcars$maketype, mra)
clcars$displ=as.double(gsub(",",".",displ))


par(mfrow=c(1,1))
hist(clcars$displ, breaks = 20)
boxplot(clcars$displ)
ffdispl=as.data.frame(table(clcars$displ))
str(ffdispl)

plot(clcars$displ,clcars$price)


# FE
# create age-cats using cut-command - one less label
summary(clcars$age)
label=c("veteran","old","normal","new")
br=c(Inf,25,10,5,1)
clcars$agecat=cut(clcars$age,labels = label,breaks = br)
barplot(table(clcars$agecat))



# EDA
# numeric - put in sep dataframe
selv=sapply(clcars, is.numeric)
dfnum=clcars[,selv]
colnames(dfnum)
vn=colnames(dfnum)
length(vn)
par(mfrow=c(3,3))
par(mfrow=c(1,1))

for (var in vn) {
  boxplot(clcars[[var]], xlab=var)
}

# categorial - put in sep dataframe vha sapply
selv=sapply(clcars, is.numeric)
cselv=!selv
dfcat=clcars[,cselv]
dfcat=dfcat[, -5]
colcat=list()
catnames=colnames(dfcat)
for (name in catnames ) {
  tmpct=table(dfcat[[name]])
  colcat[[name]]=tmpct
}

barplot(colcat$region)
barplot(colcat$make)

# ML
#prep
pm=lm(data=clcars, price ~ displ)
summary(pm)
pm$coefficients
abline(a=53000, b=75849)

# now for all numeric
selv=sapply(clcars, is.numeric)
dfnum=clcars[,selv]
colnames(dfnum)
price=dfnum$price
dfindep=dfnum[,-c(3,4)]
colnames(dfindep)
res=list()
vname=colnames(dfindep)
for (i in 1:length(vname)) {
  print(i)
  formular=paste0("price ~ ",vname[i])
  tmmodel=lm(data=clcars, formula = formular)
  res[[i]]=tmmodel$coefficients
}
par(mfrow=c(2,2))
for (i in 1:length(vname)) {
  param=vname[i]
  plot(clcars[[param]], clcars$price)
  abline(a=res[[i]][1], b=res[[i]][2], col="red")
}
  

# non-numeric - put in sep dataframe
nselv=!selv
dfchar=clcars[,nselv]

# aggregation
# mean-mpg pr type
mkml=aggregate(data=clcars, mpg ~ make, FUN=mean)
mkml=aggregate(data=clcars, mpg ~ make+agecat, FUN=length)
mkml2=aggregate(data=clcars, mpg ~ make+agecat, FUN=mean)

ggplot(mkml2, aes(x=make,y=mpg, fill=agecat))+
  geom_col(position = "dodge")

