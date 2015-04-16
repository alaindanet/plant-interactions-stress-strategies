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
data$com <- as.factor(data$com)
tapply(data$growth, list( community=data$com, microsite=data$ms), length)


## 1. Simple: simulate differences between treatments in open sites.

open <- data[ which(data$ms =="o"),]
vmean <- seq(0,4, by=0.5) # Means of growth tested
vsd <- seq(2,4, by=0.4) # Standard deviations tested
diff <- 0.1 # Means differences
n <- 18 # number of data replicates
r <- 30 # number of data generations

iterations <- expand.grid(list(mean=vmean,
                               sd=vsd,
                               replicate= seq(1,r,1))
                          )

### Data collector
result <- as.data.frame(list(ms="o",
                             rep= iterations$replicate,
                             mean= iterations$mean,
                             sd= iterations$sd,
                             diff= diff,
                             slope1=NA,
                             slope2=NA,
                             slope3=NA,
                             rsqr=NA
                             )
                        )

### Loop

for (i in 1:nrow(iterations)){
    open[which(open$com==1),]$growth <- rnorm(n=n, mean=iterations$mean[i], sd=iterations$sd[i])
    open[which(open$com==2),]$growth <- rnorm(n=n, mean=iterations$mean[i] - diff, sd=iterations$sd[i])
    open[which(open$com==3),]$growth <- rnorm(n=n, mean=iterations$mean[i] + diff, sd=iterations$sd[i])
    open[which(open$com==4),]$growth <- rnorm(n=n, mean=iterations$mean[i] - 2 * diff, sd=iterations$sd[i])
    
    result[i, c("rep","mean","sd","slope1","slope2","slope3")] <- c( iterations[i,c("replicate","mean","sd")],
                                                                     summary(lm(growth~com,open))$coefficients[2:4, 1])
  }

### Result
hist(result$slope)
plot(rsqr~sd,result)

### Mean by parameter combinations
> tapply(result$slope1, list(mean=result$mean, sd=result$sd),function(x) hist(x))


res <- 