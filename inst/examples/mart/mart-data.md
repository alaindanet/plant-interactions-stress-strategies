---
title: "Grazing data from Mart"
output:
html_document:
    toc: true
    theme: united
---

```
setwd("../thesis_chap2_alicante/")

data <- read.table(file="Mart_data.csv", sep=";" , dec=",", header=TRUE)

# We want this variables as factor because there are not continious
factor <- c("terrace", "treatment", "watering", "rabbit", "plot", "microsite")
for (i in 1:length(factor)) {
	data[,factor[i]] <- as.factor(data[,factor[i]])	
}

# Variable description:
# -treatment (0, no, 1 low, 2 high, 3 rabbit))
# -rabbit grazed 1=yes, 0=no
# -microsite 1=patch/0=open
# -survival (0=dead, 1=alive)

# We also want have the mean size reduction of Antillys because of grazing, so we keep only the Antyllis which were alive.
tapply(as.numeric(data$survival),
       INDEX=list(terrace=data$terrace,
		  microsite=data$microsite),
       FUN=mean)
data <- data[which(data$survival==1),]

# Too much mortality in terrace 1, less than 10% survived, I exclude it from analysis
data <- data[-which(data$terrace==1),]
# We want to mimic the highest grazing treatment:
data <- data[which(data$treatment==2| data$treatment==0),]

tapply(data$survival,
       INDEX=list(terrace=data$terrace,
		  microsite=data$microsite),
       FUN=length)

# Data visualisation
# pdf(file="QQ_plot.pdf", paper="a4r")
par(mfrow=c(1,3))
qqnorm(data$lnheightmay2014, main = "QQ-plot - May height")
qqline(data$lnheightmay2014)
qqnorm(data$lnheightoct14, main = "QQ-plot - October height")
qqline(data$lnheightoct14)
qqnorm(I(data$lnheightmay2014-data$lnheightoct14), main = "QQ-plot - diff height")
qqline(I(data$lnheightmay2014-data$lnheightoct14))
par(mfrow=c(1,1))
# dev.off()
# It's a bit shitty with the difference of the neperian logarithme beacause of thei property: ln(x) - ln(y) = ln(x/y)

data$may <- exp(data$lnheightmay2014)
data$oct <- exp(data$lnheightoct14)
data$diff <- data$may - data$oct
data$lndiff <- log(data$diff, base=exp(1))

par(mfrow=c(1,3))
qqnorm(data$may)
qqline(data$may)
qqnorm(data$oct)
qqline(data$oct)
qqnorm(data$diff)
qqline(data$diff)
par(mfrow=c(1,1))

# Statistical model
library(nlme)

## Dépenpence of size between the two dates
mod <- lm(lnheightoct14 ~ lnheightmay2014, data=data)
par(mfrow=c(2,2))
plot(mod)
par(mfrow=c(1,1))
mod <- lm(lnheightoct14 ~ lnheightmay2014 + microsite , data=data)

mod <- lm(lnheightoct14 ~   treatment:microsite , data=data)

mod <- lm(lnheightoct14 ~ lnheightmay2014 + treatment*microsite , data=data)

mod <- lm(lnheightoct14 ~ lnheightmay2014, data=data)
mod2 <- lm(residuals(mod) ~ data$treatment*data$microsite)
```
Fin
