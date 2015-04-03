######################################################
# Data simulation and estimability of the parameters
#
# Author: Alain Danet
#
# Date: 24/03/14
######################################################

# Tasks:
#   - Recreate my protocol
#   - Simulate data with a range of parameters
#   - Estimate the power of the experiment and the estimability of the parameters.

# Protocol

## Dataset

data <- as.data.frame(list(ms= c( rep("o", times=72),
                                  rep("p", times=72)
                                  ),
                           com= 1:4,
                           growth=NA))

str(data)
tapply(data$growth, list( community=data$com, microsite=data$ms), length)


# Simulate data with rnorm()

hist(rnorm(36, mean=1, sd=2))

## 1. Simple: simulate differences between treatments in open sites.

open <- data[ which(data$ms =="o"),]
vmean <- seq(0,4, by=0.5) # Means of growth tested
vsd <- seq(2,4, by=0.4) # Standard deviations tested
diff <- 0.1 # Means differences
n <- 18 # number of replicates

iterations <- expand.grid(list(mean=vmean,
                               sd=vsd)
                          )

### Data collector
result <- as.data.frame(list(ms="o",
                             mean= iterations$mean,
                             sd= iterations$sd,
                             diff= diff,
                             slope=NA,
                             rsqr=NA
                             )
                        )

### Loop

for (i in 1:nrow(iterations)){
    open[which(open$com==1),]$growth <- rnorm(n=n, mean=iterations$mean[i], sd=iterations$sd[i])
    open[which(open$com==2),]$growth <- rnorm(n=n, mean=iterations$mean[i] + diff, sd=iterations$sd[i])
    open[which(open$com==3),]$growth <- rnorm(n=n, mean=iterations$mean[i] + diff, sd=iterations$sd[i])
    open[which(open$com==4),]$growth <- rnorm(n=n, mean=iterations$mean[i] - diff, sd=iterations$sd[i])
    
    result[i, c("slope", "rsqr")] <- summary(lm(growth~com,open))$coefficients[2,c(1,4)]
  }

### Result

hist(result$slope)
plot(rsqr~slope,result)
