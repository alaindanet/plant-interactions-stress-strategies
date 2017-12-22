####################################################################
#		 Pre-design of experiment
# Author: Alain Danet
# Created at 02/13/2015
# Tasks:
# - Build data.frame to attribute random placements of saplings in the experiment
# - Test estimability of parameters
#
#####################################################################

# library
library(vimcom)

# TODO: Define the cutting with Mart's data
#Treatment code: 0,1,2,3; no grazed, low, high, rabbit 

# Analysis of data from Mart
data <- read.table("~/Documents/Mart_data.csv", #Data from data
		   sep=";",
		   dec=",",
		   header=T)
data$treatment <- as.factor(data$treatment)
data$microsite <- as.factor(data$microsite)
data$terrace <- as.factor(data$terrace)
tapply(data$survival, list(terrace=data$terrace), mean)  #Survival by terraces 
tapply(data$label, list(terrace=data$terrace), length)  #number of sites by terraces 
tapply(data$label, list(terrace=data$plot), length) #Nb site by terraces 

# Select plants who survived after plantation
data <- data[data$survival!=0,]
hist(I( data$lnheightmay2014 - data$lnheightoct14 ))

# Model: parameter estimations
# NOTE: data were transformed in neperian logarithm. It's the "reciproque" function of the exponential one.

# data visualisation# {{{
par(mfrow=c(2,2))
qqnorm(data$lnheightmay2014); qqline(data$lnheightmay2014)
text(x=-3,y=2, labels=c("Ln May 2014"), pos=4)
qqnorm(data$lnheightoct14); qqline(data$lnheightoct14)
text(x=-3,y=2, labels=c("Ln Oct 2014"), pos=4)
qqnorm(exp( data$lnheightmay2014 )); qqline(exp( data$lnheightmay2014 ))
text(x=-3,y=10, labels=c("May 2014"), pos=4)
qqnorm(exp( data$lnheightoct14 )); qqline(exp( data$lnheightoct14 ))
text(x=-3,y=10, labels=c("Oct 2014"), pos=4)
par(mfrow=c(1,1))
# }}}

mod <- lm(I(lnheightmay2014 - lnheightoct14) ~ microsite + treatment,data)
summary(mod)

mod <- lm(I(lnheightmay2014 - lnheightoct14) ~ microsite:treatment,data)
summary(mod)
