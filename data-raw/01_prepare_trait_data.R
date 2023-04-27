########################
#  Prepare trait data  #
########################

library(tidyverse)
library(magrittr)
library(here)

mass_data <- read_csv2(here("data-raw", "data_mars_traits.csv"))
mass_data_lg <- mass_data %>%
  pivot_longer(
    cols = sp1:dry_m_f3,
    names_to = c(".value", "ind"),
    names_pattern = "(.*)(.)"
  )
total_mass <- mass_data_lg %>%
  select(ter, plot, label, com, sp, ind, fresh_mass, dry_mass) %>%
  mutate(across(c(ter, plot, label, com, sp, ind), as.factor)) %>%
  na.omit()

leaf_file_path <- here(
  "data-raw",
  "leafs",
  "mars",
  "empty_file_for_traits-2.csv")
leaf_data <- read_csv2(leaf_file_path)

total_leaf_area <- leaf_data %>%
  select(ter, plot, label, com, sp, ind, totalleaf_surface) %>%
  mutate(across(c(ter, plot, label, com, sp, ind), as.factor)) %>%
  na.omit()

# The leaf area for the third individual have not been measured
mass_leaf_area <- total_mass %>%
  left_join(total_leaf_area)

load(here("data", "holes_data.rda"))
treatment_label <- holes_data %>%
  mutate(label = as.factor(label)) %>%
  group_by(label, ms, com, watering) %>%
  summarise(n = n(), .groups = "drop") %>%
  select(-n)

sla_ldmc <- mass_leaf_area %>%
  mutate(
    ldmc = dry_mass / fresh_mass,
    sla = totalleaf_surface / dry_mass
  ) %>%
select(-com) %>%
left_join(treatment_label)


sla_ldmc %>%
  ggplot(aes(y = ldmc, x = sp, color = ms, linetype = com)) +
  geom_boxplot() +
  facet_grid(cols = vars(watering))


sla_ldmc %>%
  group_by(ter, sp, ms, com, watering) %>%
  summarise(
    var_ldmc = var(ldmc), n = n()) %>%
  ggplot(aes(y = var_ldmc, x = sp, color = ms, linetype = com)) +
  geom_boxplot() +
  facet_grid(cols = vars(watering))
