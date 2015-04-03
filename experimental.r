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

# Enter data for the terrace

data <- as.data.frame(matrix(data=NA, ncol=5, nrow=80*3))
colnames(data) = c("plot", "site", "number", "no stick", "no label")
data$number <- c( seq( 360, 400, by=1),
		 seq( 960, 997, by=1),
		 seq( 281, 359, by=1),
		 seq( 1451, 1531, by=1),
		 "X"
		 )
data2  <- edit(data)
write.table(data2, file="data.csv",sep=";",dec=".")
#

# First data
data  <- read.table("data.csv", sep=";", dec=".", header=T)
str(data)                               
summary(data)

data$treatment  <- data$no.stick
replace(data$treatment, sample(x=1:240, size=240, replace=F), 1:16) # Assign random treatments to the microsites

data[which(data$site=="p"), ]$treatment <- replace(data[which(data$site=="p"), ]$treatment, sample(x=1:120, size=120, replace=F), 1:16)


# Create a data.frame with all variables
# ?data.frame()
# Number of columns: ID, site, terrace, water, grazing, community, replicates
# Number of treatments: 2 sites*2 grazing level*2 water level*4 communities
# Number of microsites: 32 treatments * 8 replicates per terraces * 4 terraces
# Number of saplings: 32*8*4*3
data <- as.data.frame(matrix(data=NA, ncol=8, nrow=32*8*4*3)); colnames(data) = c("ID", "terrace", "site", "water", "grazing", "community", "replicates", "place") # 
str(data)
# ?factor()
data$ID <- as.integer(1:length(data$ID))
# ?rep()
data$terrace <- rep(c(1:4), times=nrow(data)/4); data$terrace <- as.factor(data$terrace)
data$site <- rep(c("open", "patch"), times=nrow(data)/2); data$site <- as.factor(data$site)
data$water  <- rep(c("watering", "control"), times=nrow(data)/2); data$water <- as.factor(data$water)



# Analysis of data from Mart
mart <- read.table("~/Documents/Mart_data.csv", #Data from mart
		   sep=";",
		   dec=",",
		   header=T)
str(mart)

tapply(mart$survival, list(terrace=mart$terrace), mean)  #Survival by terraces 
tapply(mart$label, list(terrace=mart$terrace), length)  #number of sites by terraces 
tapply(mart$label, list(terrace=mart$plot), length) #Nb site by terraces 

# So, we will use the first terrace for the pilote because there is the lowest survival

#Treatment code: 0,1,2,3; no grazed, low, high, rabbit 
ter1  <- mart[mart$terrace==1,]        #select first terrace 
tapply(ter1$microsite,
       list(plot=ter1$plot,
	    microsite=ter1$microsite),
       length)                         # Miss 2 sites in plot 2
# Mart confirmed some microsites were missing from the start and some have been lost during the experiment. 

# Tirer les plots au hasard
str(ter1)
ter1$treatment  <- NA
ter1bis <- ter1[ter1$microsite==1,]
ter1bis  <- ter1bis[sample(x=1:128, size=128, replace=F),]

ter1[which(ter1$microsite=="0"), ]$treatment <- replace(ter1[which(ter1$microsite=="0"), ]$treatment, sample(x=1:128, size=128, replace=F), 1:32)


hist(ter1$treatment)
# Terrace1 - Choix des patchs à utiliser
data  <- read.csv("terrace1.csv", sep=",", header=T)
str(data)
patch<-data[data$microsite=="p",]
nrow(patch)
summary(patch$nurse_dead)
patch <- patch[which(is.na(patch$nurse_dead)),]
nrow(patch)
patch <- patch[which(is.na(patch$creuser)),]
nrow(patch)
patch <- patch[patch$label!="NA",]
#I already dig 7 patch, so I need 121
select  <- patch[sample(x=1:121, size=121, replace=F),]
select[,c("plot","label","microsite")]

# Patch label we already dig:
select <- c(4,6,10,11,20,25,37,128,121,60,1283,151,1272,112,58,63,175,1305,77,1262,148,179,145,36,1253,1320,123,1266,1,1275,1268,82,177,213,1327,233,174,157,54,1286,78,71,30,218,231,1258,153,64,161,222,156,200,149,238,1251,176,139,237,130,202,165,181,101,1280,136,228,1261,126,103,28,243,132,1311,1265,185,1326,9,119,47,73,169,164,191,1285,52,1252,1267,31,106,1315,240,108,42,35,84,160,167,172,1278,133,92,1270,85,69,23,178,1282,88,48,166,170,1294,22,86,129,32,39,1313,14,1256,224,1290,1299,1300,192,91,110) # 
todo  <- patch[-patch$label %in% select,]
todo$label

#Community treatment:
## 1.Antilis
## 2.Atriplex
## 3.Pistachia
## 4.Mixture
patch$treatment <- NA
patch$treatment <- replace(patch$treatment, sample(x=1:nrow(patch), size=nrow(patch), replace=F), 1:4) # 

length(which(patch$treatment==4))
patch[,c("plot","label","treatment")]

# Terrace1: choix des traitements
data  <- read.csv("terrace1ok.csv", sep=",", header=T, na.string="")
str(data)

tapply(data$patch,
       list(plot=data$plot),
       length) 
data <- rbind(data,c("new","new","new",NA))
data$treatment <- replace(data$treatment, sample(x=1:nrow(data), size=nrow(data), replace=F), 1:4) # 

length(which(data$treatment==1))
data[,]
write.table(data, file="terrace1beta.csv", sep=",", col.names=T, row.names=F)
?write.table

data <- read.table("test.csv", sep=",", header=T)
str(data)
data[data$treatment==3,]

data  <- read.csv("terrace1ok_checked.csv", sep=",", header=T, na.string="")
str(data)

tapply(data$patch,
       list(com=data$treatment),
       length) 

