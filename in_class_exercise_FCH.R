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

# Examine our dataset, with color-coding by batID:
ggplot(record_dat) + 
  aes(x = boatnoise, y = songlength, color=sensorid) + 
  geom_point() + # Color code observations by batID
  theme_bw() +
  ylab("Whale Song Length (s)") +
  xlab("Boat Noise (dB)")
ggplot(record_dat) + 
  aes(x = boatnoise, y = songlength, color=dayid) + 
  geom_point() + # Color code observations by batID
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
predictions(poolsongmod, newdata=data.frame(environ.ppt=c(2, 4),
                                         genotype=c("A")))

## For easy visualization, nice to plot just the taillength coefficient:
mcmc_plot(poolmod, variable=c("b_taillength"))
plot_predictions(poolmod, condition="taillength") + theme_bw()

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


## PART 3: PARTIAL POOLING #####
## Fit a multilevel model with a varying intercept for BatID:
varyintsongmod <- brm(songlength ~ boatnoise + watertemp + boatactivity +
                (1|sensorid), ## this is our varying intercept 
              data=record_dat,
              family="Gamma"(link=log))
plot(varyintsongmod)
plot_predictions(varyintsongmod, condition = c("boatnoise")) + theme_bw()
# Examine the summary table: What is new here? What does it mean?
summary(varyintsongmod)
# Multilevel hyperparameters? standard deviation of the intercepts? describes around
# mean intercept, what is spread of data, if it is really small, suggests that 
# bats are all behaving differently
# Group level effects are varying parameters, population level effects are fixed effects

## Examine the posterior: What is new here? What does it mean?
posterior <- data.frame(mixmod)
#has all different intercepts and predictions
# Examine the coefficients from the model:
mcmc_plot(varyintsongmod,pars=c("^b", "^sd")) ## Just plots the "fixed" effects and the sd for the varying intercept (which begin with the strings "b_" or "sd")
plot_predictions(varyintsongmod, condition = c("boatnoise", "sensorid")) + theme_bw()
bayes_R2(varyintsongmod)
## The lines below will just show our varying intercepts (which all begin with the string "r_").
mcmc_plot(varyintsongmod, pars = c("^r_")) 
#fastest bats are 2 and 3 because capture time is less than mean so faster
#slowest bat is 10 because capture time is longer
ranef(mixmod) # or we can print them instead, with ranef.
#tells each bats departure from mean intercept (positive or negative)

## DISCUSS PART 3: 
## What is the biological interpretation of the varying intercepts? Discuss some patterns you see in their estimates.
# could say that some bats faster than others


## PART 4: VARYING SLOPES ####

## Add a varying slope to your original mixed effects model and view the coefficients. Discuss with a partner.

mixmod2 <- brm(captime ~ taillength + (1+taillength|batID), 
               data=batdat,
               family="Gamma"(link=log))

summary(mixmod2)

mcmc_plot(mixmod2, pars = c("^r_")) 

ranef(mixmod2)

## DISCUSS PART 4: 
## What is the biological interpretation of the varying SLOPES? Discuss some patterns you see in their estimates.
#Shallower slopes (more negative) show less of an impact of tail length on capture time,
#steeper slopes (more positive) show more of an impact of tail length on capture time.










## BONUS: Marginal effects #####
## Communicating the mean pattern v. the group-level patterns?

conditional_effects(mixmod2) ## brms has a "marginal effects" plotting function built in, but the marginaleffects package works here still too!
plot_predictions(mixmod2, condition = c("taillength")) + theme_bw()

## The plots above show the mean prediction, across all bats, however we can also plot each "bat-level" prediction. Why or why would we NOT want to do this?
plot_predictions(mixmod2, condition = c("taillength", "batID")) + theme_bw()

