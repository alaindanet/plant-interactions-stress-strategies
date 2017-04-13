#'#'#########################################
#'  Functions to manipulate the data sets  #'
#'  Date: 22-03-2016
#'  Author: Alain Danet
#'##########################################'

library(tidyverse)
library(stringr)
library(testthat)

# Testing analysis 
test_file("./tests/test_data.r")

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

# Load data
january <- read_csv2("../data/data_january_plant.csv") %>%
    mutate(date = "jan")
march <- read_csv2("../data/data_mars_plant.csv") %>%
    mutate(date = "mar")
june <- read_csv2("../data/data_juin_plant.csv") %>%
    mutate(date = "jun")
november <- read_csv2("../data/data_nov_plant.csv") %>%
    mutate(date = "nov") # The table are quite different

# Which table do we want ?
## 1. Survival table
## 2. Quantitative data table (height and diameter)

####################
#  Survival table  #
####################

# In january, there is no dead because it was the starting point

# How are noticed the dead individuals in March ?
march %>% filter(h1 == "Dead")
unique(c(march$h1, march$h2, march$h3)) # Dead are noticed "Dead" and broken are
#noticed "C"

# Transform data
var_joined_by <- c("ter", "plot", "label", "ms", "com", "watering", "ind", "date", "notes")
## species table
march_sp <- march %>%
    select(-h1, -h2, -h3, -d1, -d2, -d3, -hm1, -hm2, -hm3) %>%
    gather(sp1, sp2, sp3, key = "ind", value = "species") %>%
    mutate(ind = str_replace(ind, "sp", ""))
## height table
march_h <- march %>%
    select(-sp1, -sp2, -sp3, -d1, -d2, -d3, -hm1, -hm2, -hm3) %>%
    gather(h1, h2, h3, key = "ind", value = "h") %>%
    mutate(ind = str_replace(ind, "h", ""))
## maximum height table
march_hm <- march %>%
    select(-sp1, -sp2, -sp3, -d1, -d2, -d3, -h1, -h2, -h3) %>%
    gather(hm1, hm2, hm3, key = "ind", value = "hm") %>%
    mutate(ind = str_replace(ind, "hm", ""))
## diameter table
march_d <- march %>%
    select(-sp1, -sp2, -sp3, -h1, -h2, -h3, -hm1, -hm2, -hm3) %>%
    gather(d1, d2, d3, key = "ind", value = "d") %>%
    mutate(ind = str_replace(ind, "d", ""))
## full join
march <- full_join(march_sp, march_h, by = var_joined_by) %>%
    full_join(., march_d, by = var_joined_by) %>%
    full_join(., march_hm, by = var_joined_by)

# Are dead correctly recorded in h, d, hm ?
sort_survival <- function(x, dead = "Dead", casse = "C"){
ifelse(x %in% c(dead, casse), ifelse(x == dead, "D", "C"), 1)
}
mar_surv <- march %>%
    mutate(
	eh = sort_survival(h),
	ed = sort_survival(d),
	ehm = sort_survival(hm))

mar_surv %>% filter(eh == "D", ed == "D", ehm == "D") %>% nrow(.)
mar_surv %>% filter(eh == "D") %>% nrow(.)
mar_surv %>% filter(ehm == "D") %>% nrow(.)
mar_surv %>% filter(ed == "D") %>% nrow(.) # Missing registered death in diameter columns
# Check that dead recorded are the same in h and hm:
mar_surv %>% filter(eh == "D", ehm == "D") %>% nrow(.) ## There are, nice!
## Take death from the eh or ehm column
replace_numerise <- function(x, pattern = "Dead|C", replacement = "NA", dec = ","){
    temp <- str_replace(x, pattern, replacement)
    res <- as.numeric(str_replace(temp, dec, "."))
    # Check that no extra NA are introduced
    expect_equal(sum(temp == "NA", is.na(temp), na.rm = T), sum(is.na(res)))
}
mar_surv2 <- mar_surv %>%
    rename(survival = eh) %>%
    mutate(
	broken = ifelse(survival == "C", 1, 0), # Separate natural death from accidental breaking
	survival = ifelse(survival == "D", 0, 1),
	h = replace_numerise(h),
	d = replace_numerise(d),
	hm = replace_numerise(hm)
	) %>%
    select(-ed, -ehm) # Do not need anymore

