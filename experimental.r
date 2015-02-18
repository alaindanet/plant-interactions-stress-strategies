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

# Create a data.frame with all variables
# ?data.frame()
# Number of columns: ID, site, terrace, water, grazing, community, replicates
# Number of treatments: 2 sites*2 grazing level*2 water level*4 communities
# Number of microsites: 32 treatments * 8 replicates per terraces * 4 terraces
# Number of saplings: 32*8*4*3
data <- as.data.frame(matrix(data=NA, ncol=8, nrow=32*8*4*3)); colnames(data) = c("ID", "terrace", "site", "water", "grazing", "community", "replicates", "place")
str(data)
# ?factor()
data$ID <- as.integer(1:length(data$ID))
# ?rep()
data$terrace <- rep(c(1:4), times=nrow(data)/4); data$terrace <- as.factor(data$terrace)
data$site <- rep(c("open", "patch"), times=nrow(data)/2); data$site <- as.factor(data$site)
data$water  <- rep(c("watering", "control"), times=nrow(data)/2); data$water <- as.factor(data$water)


