get_treatment_by_label <- function(x = growth_raw_data){
  x %>%
    mutate(label = as.factor(label)) %>%
    group_by(label, ms, com, watering) %>%
    summarise(n = n(), .groups = "drop") %>%
    select(-n)
}

get_leaf_trait_data <- function(
  mass_data = leaf_mass_raw_data,
  leaf_data = leaf_raw_data) {

  total_mass <- mass_data %>%
    pivot_longer(
      cols = sp1:dry_m_f3,
      names_to = c(".value", "ind"),
      names_pattern = "(.*)(.)") %>%
  select(ter, plot, label, com, sp, ind, fresh_mass, dry_mass) %>%
  mutate(across(c(ter, plot, label, com, sp, ind), as.factor)) %>%
  na.omit()
total_leaf_area <- leaf_data %>%
  select(ter, plot, label, com, sp, ind, totalleaf_surface) %>%
  mutate(across(c(ter, plot, label, com, sp, ind), as.factor)) %>%
  na.omit()

# The leaf area for the third individual have not been measured
mass_leaf_area <- total_mass %>%
  left_join(total_leaf_area)

mass_leaf_area %>%
  mutate(
    ldmc = dry_mass / fresh_mass,
    sla = totalleaf_surface / dry_mass
    ) %>%
select(-com) %>%
rename(species = sp)


}
