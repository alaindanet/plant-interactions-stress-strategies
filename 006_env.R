################################################################################
#                          Analyze environmental data                          #
################################################################################

library('tidyverse')
library('magrittr')

january <- read_csv2("data-raw/data_january_env.csv") %>%
    mutate(date = "jan")
march <- read_csv2("data-raw/data_mars_env.csv") %>%
    mutate(date = "mar")
june <- read_csv2("data-raw/data_juin_env.csv") %>%
    mutate(date = "jun")
november <- read_csv2("data-raw/data_nov_env.csv") %>%
    mutate(date = "nov") # The table are quite different

january %<>% select(ter, plot, label, ms, com, water, date, temperature)
march %<>% select(ter, plot, label, ms, com, water, date, temperature)
june %<>% select(ter, plot, label, ms, com, water, date, temperature)
november %<>% select(ter, plot, label, ms, com, water, date, temperature)
data <- rbind(january, march, june, november) %>%
  mutate_at(vars(c("ms", "com", "water")), as.factor) %>%
  mutate(temperature = as.numeric(temperature))

paired_holes <- pair_find(data) %>%
  mutate(pair_id = as.numeric(rownames(.))) %>%
  gather(type, label, Open, Patch)

data <- left_join(data,
  select(paired_holes, label, pair_id)
)

data %>%
  select(-label) %>%
  spread(ms, temperature) %>%
  group_by(date) %>%
  rename(open = `0`, patch = `1`) %>%
  summarise(open = mean(open, na.rm = TRUE), patch = mean(patch, na.rm = TRUE))
  
