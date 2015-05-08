rm(list = ls())

library(reshape)
library(plyr)
library(DataCombine) # Kick-ass time series and data management package
library(lme4) # Random effects package
library(ggplot2)



#######################
### DATA MANAGEMENT ###
#######################

## JOB APPROVAL DATA


setwd("C:\\Users\\Sheryl\\Documents\\PSC 536\\Research Paper")
JAR <- read.csv("JAR Pres.csv")

J <- JAR[JAR$QUESTION == 1,]
J <- unique(J)
J <- J[complete.cases(J[,8]),]

pres.app <- aggregate(J[,9], by = list(J$STATE, J$YEARIN), mean)
rm(JAR)
rm(J)

# Renamed Group.1 to State, Group.2 to Year, and x to approval
names(pres.app) <- c("state", "year", "approval")

# Creat GeoFips for merging
pres.app$GeoFips <- pres.app$state*1000


## STATE PERSONAL INCOME DATA


state.inc <- read.csv("StateIncPerCapPercentChange.csv", na.strings = "(NA)")

# Arrange the data by state and year observations
melted <- melt(state.inc, id.vars = c(1:2))

# Use regular expressions to remove "X" from the beginning of each year 
melted$variable <- as.character(melted$variable)
melted$year <- gsub("^X", replacement = "", melted$variable)

state.inc <- melted[,c(-3)]
rm(melted)
state.inc <- rename(state.inc, c(value = "perinc"))


## STATE MACROPOLITY DATA

state.opinion <- read.csv("Enns_Koch_StateOpinionData.csv")
state.opinion <- rename(state.opinion, c(statename = "GeoName"))

# Macropolity

state.opinion$macropolity <- (state.opinion$democrat/(state.opinion$democrat + 
                                                        state.opinion$republican))*100

# Lag macropolity

# order

state.opinion <- state.opinion[order(state.opinion$GeoName, state.opinion$year),]

state.opinion <- slide(state.opinion, Var = "macropolity", GroupVar = "GeoName", 
                       NewVar = "lagmacro", slideBy = -1, reminder = FALSE)

write.csv(state.opinion, file = "StateMacropolityData.csv", row.names = FALSE)

## CONSUMER SENTIMENT ##

natcs <- read.csv("natcs.csv")


## Merging

# pres.app + state.inc, approval and income joined

app.inc <- join(state.inc, pres.app, by = c("GeoFips", "year"))

# app.inc + state.opinion, previous joined with macropolity

macro <- join(app.inc, state.opinion, by = c("GeoName", "year"))

# add national consumer sentiment index

macro <- join(macro, natcs, by = "year")

# remove old data

rm(app.inc, pres.app, state.inc)

# order

macro <- macro[order(macro$GeoName, macro$year),]

# Make party variable

macro$party[macro$year <= 1932] <- -1
macro$party[macro$year >= 1933 & macro$year <= 1952] <- 1
macro$party[macro$year >= 1953 & macro$year <= 1960] <- -1
macro$party[macro$year >= 1961 & macro$year <= 1968] <- 1
macro$party[macro$year >= 1969 & macro$year <= 1976] <- -1
macro$party[macro$year >= 1977 & macro$year <= 1980] <- 1
macro$party[macro$year >= 1981 & macro$year <= 1992] <- -1
macro$party[macro$year >= 1993 & macro$year <= 2000] <- 1
macro$party[macro$year >= 2001 & macro$year <= 2008] <- -1
macro$party[macro$year >= 2009] <- 1


###########################
### Plot of Macropolity ###
###########################

state.plot.data <- state.opinion[ which (state.opinion$GeoName != "D.C."),]

state.macro <- ggplot() + geom_line(data = state.plot.data, aes(x = year, y = macropolity)) + 
                                    facet_wrap( ~ GeoName, ncol = 5) + 
                                    ylab("Macropartisanship") +
                                    xlab("Year: 1956-2010") + theme(axis.text.x = element_blank(),
                                                         axis.ticks.x = element_blank())

state.macro # export as pdf with 6 by 9 size

##############
### MODELS ###
##############


## Everything

alltry.re <- lmer(macropolity ~ lagmacro + perinc*party + party + approval*party + approval +
                                   (1 + lagmacro |GeoName), data = macro)

summary(alltry.re)
coefs <- coef(alltry.re)

# Get Table for State b's
state.list <- coefs$GeoName[,1:2]
states <- row.names(state.list)
b <- state.list[,2]
b <- round(b, digits = 2)
state.b <- as.data.frame(cbind(states, b))
state.b[order(state.b$b),]

fixef(alltry.re)
ranef(alltry.re)


## Just economy

econ.re <- lmer(macropolity ~ lagmacro + perinc*party + party + (1 + lagmacro |GeoName), 
                  data = macro)

summary(econ.re)
econ.coefs <- coef(econ.re)

# Econ Coefs Table
state.econ.list <- econ.coefs$GeoName[,1:2]
states.econ <- row.names(state.econ.list)
b.econ <- state.econ.list[,2]
b.econ <- round(b.econ, digits = 2)
state.econ.b <- as.data.frame(cbind(states.econ, b.econ))
state.econ.b[order(state.econ.b$b.econ),]

## Regress on lag

lag.re <- lmer(macropolity ~ lagmacro + (1 + lagmacro | GeoName), data = macro)

summary(lag.re)
fixef(lag.re)
coef(lag.re)
ranef(lag.re)


### Extra Stuff

fits <- lmList(macropolity ~ lagmacro + perinc*party + approval*party | GeoName, data = macro,
               na.action = na.omit)
fits
blah <- ldply(fits, function(x) summary(x)$sigma)

econ.fits <- lmList(macropolity ~ lagmacro + perinc*party | GeoName, 
                    data = macro, 
                    na.action = na.omit)
summary(econ.fits)

macro.fits <- lmList(macropolity ~ lagmacro | GeoName, data = macro)
macro.fits
