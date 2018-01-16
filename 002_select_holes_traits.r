#'#'####################################################################
#'                              Random                                #'
#'                   Select holes to recolt leaves                    #'
#'#####################################################################'

# 
set.seed(1246)

#Load a data set

data <- read.table("../data/raw/data_january_plant.csv", sep=";", dec=",", header=T)

#by terraces

terrace  <- c(3,4)
sample_size <- c(4,4)
label_sampled <- c()

for (i in 1:length(terrace)) {
	temp <- data[data$ter==terrace[i],]
	for (j in 1:length(unique(data$ms))) {
		for (k in 1:length(unique(data$com))) {
			for (l in 1:length(unique(data$watering))) {
				print( paste(k, length(temp[temp$ms==unique(data$ms)[j] & temp$com==unique(data$com)[k] & temp$watering==unique(data$watering)[l], ]$label), sep=","))
				label_sampled_temp <- sample(temp[temp$ms==unique(data$ms)[j] & temp$com==unique(data$com)[k] & temp$watering==unique(data$watering)[l], ]$label, size=sample_size[i])
				label_sampled <- c(label_sampled, label_sampled_temp)

			}
		}
	}
}

data[data$label %in% label_sampled, c("ter", "plot", "label", "com", "ms", "watering")]

tapply(data$label, list(data$plot,data$ms, data$watering, data$com), length)

#write.table(data[data$label %in% label_sampled, c("ter", "plot", "label", "ms", "com", "watering")],
	#file="selected_functional_traits_terrace_3_4.csv",
	#quote=F,
	#sep=";",
	#dec=",",
	#row.names=F,
	#col.names=T)


