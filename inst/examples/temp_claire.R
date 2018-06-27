################################################################################
#                     Dataset sent to Claire to fill area                      #
################################################################################


data <- load.traits(data="~/Documents/thesis/alicante/thesis_chap2_alicante/data-raw/data_mars_traits.csv")

data[, "ldmc"] <- data[, "dry_mass"]/data[, "fresh_mass"]

# Transform numeric in factor
to.factor <- c("ter", "plot", "label","ms", "com", "watering", "sp", "ind")
for (i in 1:length(to.factor)) {
	data[,to.factor[i]] <- as.factor(data[,to.factor[i]])
}

data <- as_tibble(data)
 
# We remove useless data
test <- data %>%
  select(-watering, - ms, - fresh_m_f, -ldmc, - dry_m_f) %>%
  filter(!(is.na(fresh_mass) & is.na(dry_mass)))

write_csv(test, path = "../../data/empty_file_for_traits.csv")
