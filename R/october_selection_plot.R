#'#'############################################
#'  Hole selection for main experiment 15/10  #'
#'#############################################'

# Objective: select randomly holes for the main experiment in each terrace

# Numbers for the experiment
NBtreatment <- 32
NBcommunity <- 4
NBhole <- c()
NBplant <- 3000
NBrep <- 27

# Terrace 2,3,4

data <- read.table("/home/alain/Documents/thesis/thesis_chap2_alicante/Mart_data_octobre.csv",
	sep    = ";",                         #Treatment==3 : rabbit plot 
	dec    = ",",
	header = T)


# How many Artemisia are died?
length(which(data$Artemisia==1))

tapply(data$Artemisia, list(terrace=data$terrace), function(x){length(which(x==1))})

tapply(data$Artemisia, list(plot=data$plot), function(x){length(which(x==1))})

# Sain holes
sain <- data[-c(which(data$Artemisia==1)),]
sain1 <- sain[-c(which(sain$Notes=="Hole missing")),]
sain2 <- sain1[-which(sain1$treatment==3),]


# Sample patch in each terrace and in each plot
# The plan is to select randomly :
# 1) In each terrace, select randomly the plot which will have 1 replicate instead of 2
# 2) In each plot, select randomly the patches.

sample(sain2[which(sain2$terrace==4 & sain2$microsite==1),]$label, size= 16*7)
length(which(sain2$terrace==2 & sain2$microsite==1))
length(which(sain2$terrace==3 & sain2$microsite==1))
length(which(sain2$terrace==4 & sain2$microsite==1))

# Number of patch by plot, some plots don't have enough patch for 2 replicates
tapply(sain2[sain2$microsite==1,]$label, list(plot=sain2[sain2$microsite==1,]$plot), length)

################# Selection# {{{

# Plot 4a
plot4a <- sample(sain2[which(sain2$terrace==4 & sain2$microsite==1 & sain2$plot==13),]$label, size= 16*2)

com <- sample(rep(c("a","d","p","m"),times= 8), size = length(plot4a))
for(i in 1:50){
com <- sample(com)
}

plot4a <- as.data.frame(plot4a[order(plot4a)])
p4a <- cbind(plot4a,com)

# Plot 4b
plot4b <- sample(sain2[which(sain2$terrace==4 & sain2$microsite==1 & sain2$plot==14),]$label, size= 16*2)

COM <- sample(rep(c("A","D","P","M"),times= 8), size = length(plot4b))
for(i in 1:50){
COM <- sample(COM)
}

plot4b <- as.data.frame(plot4b[order(plot4b)])
p4b <- cbind(plot4b,COM)

# Plot 4c
plot4c <- sample(sain2[which(sain2$terrace==4 & sain2$microsite==1 & sain2$plot==15),]$label, size= 16*2)

COM <- sample(rep(c("A","D","P","M"),times= 8), size = length(plot4c))
for(i in 1:50){
COM <- sample(COM)
}

plot4c <- as.data.frame(plot4c[order(plot4c)])
p4c <- cbind(plot4c,COM)
p4c


################ Terrace 3

# We will use sain1 because we will put replicates in

# Plot 3a
plot3a <- sample(sain1[which(sain1$terrace==3 & sain1$microsite==1 & sain1$plot==9),]$label, size= 16*2)

COM <- sample(rep(c("A","D","P","M"),times= 8), size = length(plot3a))
for(i in 1:50){
COM <- sample(COM)
}

plot3a <- as.data.frame(plot3a[order(plot3a)])
p3a <- cbind(plot3a,COM)
p3a

# Plot 3b
plot3b <- sample(sain1[which(sain1$terrace==3 & sain1$microsite==1 & sain1$plot==10),]$label, size= 16)


COM <- sample(rep(c("A","D","P","M"),times= 8), size = length(plot3b))
for(i in 1:50){
COM <- sample(COM)
}

plot3b <- as.data.frame(plot3b[order(plot3b)])
p3b <- cbind(plot3b,COM)
p3b

# Plot 3c
plot3c <- sample(sain1[which(sain1$terrace==3 & sain1$microsite==1 & sain1$plot==11),]$label, size= 16*2)

COM <- sample(rep(c("A","D","P","M"),times= 8), size = length(plot3c))
for(i in 1:50){
COM <- sample(COM)
}

plot3c <- as.data.frame(plot3c[order(plot3c)])
p3c <- cbind(plot3c,COM)
p3c

# Plot 3d
plot3d <- sample(sain1[which(sain1$terrace==3 & sain1$microsite==1 & sain1$plot==12),]$label, size= 16*2)


COM <- sample(rep(c("A","D","P","M"),times= 8), size = length(plot3d))
for(i in 1:50){
COM <- sample(COM)
}

plot3d <- as.data.frame(plot3d[order(plot3d)])
p3d <- cbind(plot3d,COM)
p3d


################ Terrace 2

# We will use sain1 because we will put replicates in rabbit plot

# Plot 2a
plot2a <- sample(sain1[which(sain1$terrace==2 & sain1$microsite==1 & sain1$plot==5),]$label, size= 31) # There is only 31 replicates, it would nice to add one Artemisia

COM <- sample(rep(c("A","D","P","M"),times= 8), size = length(plot2a))
for(i in 1:50){
COM <- sample(COM)
}

plot2a <- as.data.frame(plot2a[order(plot2a)])
p2a <- cbind(plot2a,COM)
p2a

# Plot 2b
plot2b <- sample(sain1[which(sain1$terrace==2 & sain1$microsite==1 & sain1$plot==6),]$label, size= 16*2)


COM <- sample(rep(c("A","D","P","M"),times= 8), size = length(plot2b))
for(i in 1:50){
COM <- sample(COM)
}

plot2b <- as.data.frame(plot2b[order(plot2b)])
p2b <- cbind(plot2b,COM)
p2b

# Plot 2c
plot2c <- sample(sain1[which(sain1$terrace==2 & sain1$microsite==1 & sain1$plot==7),]$label, size= 16)

COM <- sample(rep(c("A","D","P","M"),times= 8), size = length(plot2c))
for(i in 1:50){
COM <- sample(COM)
}

plot2c <- as.data.frame(plot2c[order(plot2c)])
p2c <- cbind(plot2c,COM)
p2c

# Plot 2d
plot2d <- sample(sain1[which(sain1$terrace==2 & sain1$microsite==1 & sain1$plot==8),]$label, size= 16*2)

COM <- sample(rep(c("A","D","P","M"),times= 8), size = length(plot2d))
for(i in 1:50){
COM <- sample(COM)
}

plot2d <- as.data.frame(plot2d[order(plot2d)])
p2d <- cbind(plot2d,COM)
p2d

# Terrace 1

data <- read.table("terrace1_october_sampling.csv", sep=";", header=T)

# Plot 1a

plot1a <- sample(data[which(data$terrace=="1A" & data$ms==1),]$label, size= 16*2)

COM <- sample(rep(c("A","D","P","M"),times= 8), size = length(plot1a))
for(i in 1:50){
COM <- sample(COM)
}

plot1a <- as.data.frame(plot1a[order(plot1a)])
p1a <- cbind(plot1a,COM)
p1a

# Plot 1b
plot1b <- sample(data[which(data$terrace=="1B" & data$ms==1),]$label, size= 16*2)

COM <- sample(rep(c("A","D","P","M"),times= 8), size = length(plot1b))
for(i in 1:50){
COM <- sample(COM)
}

plot1b <- as.data.frame(plot1b[order(plot1b)])
p1b <- cbind(plot1b,COM)
p1b

#plot 1c
plot1c <- sample(data[which(data$terrace=="1C" & data$ms==1),]$label, size= 16*2)

COM <- sample(rep(c("A","D","P","M"),times= 8), size = length(plot1c))
for(i in 1:50){
COM <- sample(COM)
}

plot1c <- as.data.frame(plot1c[order(plot1c)])
p1c <- cbind(plot1c,COM)
p1c

# plot1d
plot1d <- sample(data[which(data$terrace=="1R" & data$ms==1),]$label, size= 16)

COM <- sample(rep(c("A","D","P","M"),times= 4), size = length(plot1d))
for(i in 1:50){
COM <- sample(COM)
}

plot1d <- as.data.frame(plot1d[order(plot1d)])
p1d <- cbind(plot1d,COM)
p1d
# }}}

# Holes for probes

## We need 10 replicates for now. Since I don't expect any difference now (november) for communities treatment, I have only microsite variable: 20 pairs of probes to put.

data <- read.table("treatment_alain.csv", sep=";", header=T)

# We will put them in terrace number 2:
data <- data[data$terrace==2,]

# sample 10 patch sites in terrace 2
sample(data[which(data$microsite==1),]$label, size= 10)
