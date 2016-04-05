#'#'#########################################
#'  Functions to manipulate the data sets  #'
#'  Date: 22-03-2016
#'  Author: Alain Danet
#'##########################################'


# Transform the january data set



LoadPlant <- function (data="../data/data_january_plant.csv") {

	data <- read.table(data, sep=";", dec=",", header=T)
	data[, c("ind1","ind2","ind3")] <- matrix(c(rep(1,nrow(data)),
			rep(2,nrow(data)),
			rep(3,nrow(data))), ncol=3)

	temp <- data[, c("ter", "plot", "label", "ms", "com", "watering")]
	temp1 <- cbind(temp, data[, c("sp1", "ind1", "h1", "d1")])
	colnames(temp1) <- c("ter", "plot", "label", "ms", "com", "watering",
		"sp", "ind", "h", "d")
	temp2 <- cbind(temp, data[, c("sp2", "ind2", "h2", "d2")])
	colnames(temp2) <- c("ter", "plot", "label", "ms", "com", "watering",
		"sp", "ind", "h", "d")
	temp3 <- cbind(temp, data[, c("sp3", "ind3", "h3", "d3")])
	colnames(temp3) <- c("ter", "plot", "label", "ms", "com", "watering",
		"sp", "ind", "h", "d")

	temp <- rbind(temp1,temp2,temp3)

	to.factor <- c("ter", "plot", "label","ms", "com", "watering", "sp", "ind")
	for (i in 1:length(to.factor)) {
		temp[,to.factor[i]] <- as.factor(temp[,to.factor[i]])
	}

	temp

}

LoadPlantMarch <- function (data="../data/data_mars_plant.csv") {

	data <- read.table(data, sep=";", dec=",", header=T, na.strings=c("Dead", "C", " "))
	data[, c("ind1","ind2","ind3")] <- matrix(c(rep(1,nrow(data)),
			rep(2,nrow(data)),
			rep(3,nrow(data))), ncol=3)

	temp <- data[, c("ter", "plot", "label", "ms", "com", "watering")]
	temp1 <- cbind(temp, data[, c("sp1", "ind1", "h1", "d1", "hm1")])
	colnames(temp1) <- c("ter", "plot", "label", "ms", "com", "watering",
		"sp", "ind", "h", "d", "hm")
	temp2 <- cbind(temp, data[, c("sp2", "ind2", "h2", "d2", "hm2")])
	colnames(temp2) <- c("ter", "plot", "label", "ms", "com", "watering",
		"sp", "ind", "h", "d", "hm")
	temp3 <- cbind(temp, data[, c("sp3", "ind3", "h3", "d3", "hm3")])
	colnames(temp3) <- c("ter", "plot", "label", "ms", "com", "watering",
		"sp", "ind", "h", "d", "hm")

	temp.fin <- rbind(temp1,temp2,temp3)

	to.factor <- c("ter", "plot", "label","ms", "com", "watering", "sp", "ind")
	for (i in 1:length(to.factor)) {
		temp.fin[,to.factor[i]] <- as.factor(temp.fin[,to.factor[i]])
	}

	return(temp.fin)

}


# Transform the mars trait data set
load.traits <- function (data="../data/data_mars_traits.csv") {

	data <- read.table(data, sep=";", dec=",", header=T)
	data[, c("ind1","ind2","ind3")] <- matrix(c(rep(1,nrow(data)),
			rep(2,nrow(data)),
			rep(3,nrow(data))), ncol=3)
	temp <- data[, c("ter", "plot", "label", "ms", "com", "watering")]

	list.ind <- list()
	for (i in 1:3) {
		colonne <- c(paste("sp",i,sep=""),
			paste("ind",i,sep=""),
			paste("fresh_mass",i,sep=""),
			paste("fresh_m_f",i,sep=""),
			paste("dry_mass",i,sep=""),
			paste("dry_m_f",i,sep="")
			)
		list.ind[[i]] <- cbind(temp, data[, colonne])
		colnames(list.ind[[i]]) <- c("ter",
			"plot",
			"label",
			"ms",
			"com",
			"watering",
			"sp",
			"ind",
			"fresh_mass",
			"fresh_m_f",
			"dry_mass",
			"dry_m_f")

	}
	rbind(list.ind[[1]], list.ind[[2]], list.ind[[3]])

}

