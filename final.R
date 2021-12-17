#mass shooting data
shooti<- read.csv("shootingsmj.csv")
head(shooti)
nrow(shooti)
ls(shooti)
shooti$age

# which states have the most mass shootings
levels(shooti$state)
table(shooti$state)

# gun law strictness data from worldpopulationreview.com
strict<- read.csv("csvData.csv")
strict

# putting the mass shooting data and the state law data together (gunv)
state<- strict$ï..State
strict<- cbind(strict, state)
strict<- subset(strict, select = -ï..State)

ls(strict)
shoot<- subset(shooti, select = c("fatalities","injured","state","year"))

gunv<- merge(shoot, strict, by = "state")
gunv

# number of mass shootings per state
eventperstate<- table(gunv$state)
eventperstate<- as.data.frame(eventperstate)
eventperstate$state<- eventperstate$Var1
eventperstate<- subset(eventperstate, select = c("Freq","state"))
eventperstate

massdata<- merge(gunv, eventperstate, by = "state")
ls(massdata)
massdata$state
massdata$stateabbrev<- state.abb[match(massdata$state,state.name)]


## Regression for state law rankings and mass shootings
# fit<- lm(Y ~ X, data)
fitmassdata1<- lm(Freq ~ lawsRank, data = massdata)
coef(fitmassdata1)

plot(y = massdata$Freq,
     x = massdata$lawsRank,
     ylab = "Mass Shooting Frequency",
     xlab = "Gun Law Ranking",
     ylim = c(0,25),
     main = "Mass Shootings vs Gun Law Ranking")
text(Freq~lawsRank, labels=stateabbrev,data=massdata, cex=0.5, font=2, pos=3)
abline(fitmassdata1)


## Regression for state law rankings and gun death rates
# fit<- lm(Y ~ X, data)
fitmassdata2<- lm(gunDeathRate ~ lawsRank, data = massdata)
coef(fitmassdata2)

plot(y = massdata$gunDeathRate,
     x = massdata$lawsRank,
     ylab = "Gun Death Rate",
     xlab = "Gun Law Ranking",
     ylim = c(0,25),
     main = "Gun Death Rate vs Gun Law Ranking")
text(gunDeathRate~lawsRank, labels=stateabbrev,data=massdata, cex=0.5, font=2, pos=3)
abline(fitmassdata2)

## regression for gun law letter grade and mass shootings
fitmassdata5<- lm(Freq ~ grade2019, data = massdata)
coef(fitmassdata5)

install.packages("broom", dependencies = T)
library(broom)
mass5coef<- tidy(fitmassdata5)
mass5coef<- mean(mass5coef$estimate[-1])

plot(y = massdata$Freq,
     x = massdata$grade2019,
     ylab = "Mass Shootings",
     xlab = "Gun Law Letter Grade",
     ylim = c(0,25),
     main = "Mass Shootings vs Gun Law grade")
text(Freq~grade2019, labels=stateabbrev,data=massdata, cex=0.5, font=2, pos=3)

## regression for gun law letter grade and gun death rates
fitmassdata6<- lm(gunDeathRate ~ grade2019, data = massdata)

mass6coef<- tidy(fitmassdata6)
mass6coef<- mean(mass6coef$estimate[-1])

plot(y = massdata$gunDeathRate,
     x = massdata$grade2019,
     ylab = "Gun Death Rate",
     xlab = "Gun Law Letter Grade",
     ylim = c(0,25),
     main = "Gun Death Rate vs Gun Law Grade")
text(gunDeathRate~grade2019, labels=stateabbrev,data=massdata, cex=0.5, font=2, pos=3)

#adding population data
population<- read.csv("state-population.csv")

pop<- population[!(population$ages == "under18"),]
pop$state<- pop$state.region
pop<- subset(pop, select = c("state","year","population"))

library(data.table)

poptable<- data.table(pop)
poptsum<- poptable[,list(population=sum(population)), by = "state"]
poptsum<- subset(poptsum, state!="USA" & state!="DC" & state!="PR")
poptsum
  
popmeandivisor<- max(pop$year) - min(pop$year)
popmean<- poptsum$population/popmeandivisor

poptsum$popmean<- popmean
pop2<- subset(poptsum, select = c("state","popmean"))
pop2$state<- state.name
pop2<- subset(pop2, select = c("popmean","state"))

massdata<- merge(massdata, pop2, by = "state")
massdata # now has average populations of all the states


## regressions for mass shootings and gun death rates by population
ls(massdata)
# fit<- lm(Y ~ X, data)
fitmassdata3<- lm(Freq ~ popmean, data = massdata)
coef(fitmassdata3)

plot(y = massdata$Freq,
     x = massdata$popmean,
     ylab = "Frequency of Mass Shootings",
     xlab = "Population Size",
     ylim = c(0,25),
     main = "Mass Shootings vs Population")
text(Freq~popmean, labels=stateabbrev,data=massdata, cex=0.5, font=2, pos=3)
abline(fitmassdata3)


#for gun death rates
fitmassdata4<- lm(gunDeathRate ~ popmean, data = massdata)
coef(fitmassdata4)

plot(y = massdata$gunDeathRate,
     x = massdata$popmean,
     ylab = "Gun Death Rate",
     xlab = "Population Size",
     ylim = c(0,25),
     main = "Gun Death Rate vs Population")
text(gunDeathRate~popmean, labels=stateabbrev,data=massdata, cex=0.5, font=2, pos=3)
abline(fitmassdata4)


# regression lawsrank population size
fitmassdata7<- lm(grade2019 ~ popmean, data = massdata)
coef(fitmassdata7)

plot(y = massdata$grade2019,
     x = massdata$popmean,
     ylab = "Gun Law Letter Grade",
     xlab = "Population Size",
     ylim = c(0,11),
     yaxt = "n",
     main = "Gun Law Grade vs Population")
text(grade2019~popmean, labels=stateabbrev,data=massdata, cex=0.5, font=2, pos=3)
axis(2, at = 1:9, labels = c("A","B+","B","C+","C","C-","D","D-","F"))
abline(fitmassdata7)



## Thank you Professor McCabe! ##



