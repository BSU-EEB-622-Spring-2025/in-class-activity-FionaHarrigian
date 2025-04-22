library(brms)
library(ggplot2)
library(tibble)
library(marginaleffects)
library(bayesplot)

record_dat <- read.csv("recordings.csv")
str(record_dat)
record_dat$watertemp<-as.numeric(record_dat$watertemp)
record_dat$sensorid<-as.character(record_dat$sensorid)
record_dat$dayid<-as.character(record_dat$dayid)
sensor_dat <- read.csv("sensorinfo.csv")
str(sensor_dat)
#batdat$batID <- as.factor(batdat$batID) #this makes numbers into factors, basically
#categories

#First question

## PART 1: COMPLETE POOLING ########################

# First, let's fit a "complete pooling" version of the glm.

# Examine our dataset, with color-coding by sensorid and dayid to check for groups:
ggplot(record_dat) + 
  aes(x = boatnoise, y = songlength, color=sensorid) + 
  geom_point() + 
  theme_bw() +
  ylab("Whale Song Length (s)") +
  xlab("Boat Noise (dB)")
ggplot(record_dat) + 
  aes(x = boatnoise, y = songlength, color=dayid) + 
  geom_point() + 
  theme_bw() +
  ylab("Whale Song Length (s)") +
  xlab("Boat Noise (dB)")

## Run a "COMPLETE POOLING" Model for this data:
poolsongmod <- brm(songlength ~ scale(boatnoise+watertemp+boatactivity), data=record_dat,
               family="Gamma"(link=log))
plot(poolsongmod)

summary(poolsongmod)
mcmc_plot(poolsongmod)
mcmc_plot(poolsongmod, pars="^b_")
plot_predictions(poolsongmod, condition = c("boatnoise")) + theme_bw()
bayes_R2(poolsongmod)
predictions(poolsongmod, newdata=data.frame(watertemp=c(-0.22, 10),
                                         boatactivity=c(1, 20),
                                         boatnoise=c(0.4, 37)))
#predictions(poolsongmod, newdata=data.frame(boatnoise=mean))
            
#total songs model by boatnoise, water temp, boat activity
totsongsmod<-brm(totsongs~boatnoise+watertemp+boatactivity, data = record_dat, family = "negbinomial" (link = "log"))
plot(totsongsmod)
summary(totsongsmod)
mcmc_plot(totsongsmod)
mcmc_plot(totsongsmod, pars="^b_")
plot_predictions(totsongsmod, condition = c("boatnoise")) + theme_bw()
bayes_R2(totsongsmod)
#check for overdispersion
pp_check(totsongsmod)+theme_bw()
predictions(totsongsmod, newdata=data.frame(watertemp=c(-0.22, 10),
                                            boatactivity=c(1, 20),
                                            boatnoise=c(0.4, 37)))

## PART 3: PARTIAL POOLING #####
## Fit a multilevel model with a varying intercept and slope for sensor id and day id:
varyintsongmod <- brm(songlength ~ boatnoise + watertemp + boatactivity +
                (1|sensorid) + (1|dayid), ## this is our varying intercept 
              data=record_dat,
              family="Gamma"(link=log))
plot(varyintsongmod)
plot_predictions(varyintsongmod, condition = c("boatnoise")) + theme_bw()
summary(varyintsongmod)
mcmc_plot(varyintsongmod)
#originally used sensorid varying effect only, using both is much more effective