rm(list = ls())

library(reshape)
library(plyr)
library(DataCombine) # Kick-ass time series and data management package
library(lme4) # Random effects package



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

rm(app.inc, pres.app, state.inc, state.opinion)

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




##############
### MODELS ###
##############


## Everything

alltry.re <- lmer(macropolity ~ lagmacro + perinc*party + party + approval*party + approval + 
                    (1|GeoName) + (1|year), data = macro)

summary(alltry.re)

alltry.fe <- lm(macropolity ~ lagmacro + perinc*party + party + approval*party + approval + 
                  as.factor(GeoName) + as.factor(year), data = macro)

summary(alltry.fe)

## Just economy

econ.re <- lmer(macropolity ~ lagmacro + perinc*party + party + (1|GeoName) + (1|year), 
                data = macro)

summary(econ.re)

econ.fe <- lm(macropolity ~ lagmacro + perinc*party + party + as.factor(GeoName) + 
                as.factor(year), data = macro)

summary(econ.fe)

## Full Model by State

ab.alltry.re <- lm(macropolity ~ lagmacro + perinc*party + party + approval*party + approval 
                   , data = macro, GeoName == "Alabama")

summary(ab.alltry.re)

ak.alltry <- lm(macropolity ~ lagmacro + perinc*party + party + approval*party + approval,
                data = macro, GeoName == "Alaska")

summary(ak.alltry)


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
