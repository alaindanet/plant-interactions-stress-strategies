
term_replacement <- function() {
  c(
    "\\(Intercept\\)" = "Intercept",
    "I\\(duration_m\\^2\\)" = "Time^2",
    "duration_m" = "Time",
    "comPoly" = "Diversity",
    "msPatch" = "Nurse",
    "wateringWatered" = "Watering",
    "speciesdorycnium" = "Dorycnium",
    "speciespistachia" = "Pistacia",
    "\\bter(.)\\b" = "Terrace\\1",
    ":" = " x\n"
  )
}

term_levels <- function() {
  c("Intercept", "Time", "Time^2", "Terrace3", "Terrace4", "Dorycnium",
    "Pistacia", "Diversity", "Nurse", "Watering",  "Diversity x\nNurse",
    "Diversity x\nWatering", "Nurse x\nWatering",
    "Diversity x\nNurse x\nWatering"
  )
}

response_replacement <- function() {
  c(
    "d" = "Basal diameter",
    "survival" = "Survival",
    "log_d" = "Basal diameter",
    "h" = "Vegetative height",
    "log_h" = "Vegetative height",
    "hm" = "Max vegetative height",
    "bm" = "Total biomass",
    "ldmc" = "Leaf Dry Matter Content",
    "sla" = "Specific Leaf Area",
    "temperature" = "Temperature",
    "soil.moisture" = "Soil moisture",
    "PAR" = "PAR"
  )
}

species_replacement <- function(type = "short") {
  genra <- c(
    "anthyllis" = "Anthyllis",
    "dorycnium" = "Dorycnium",
    "pistacia" = "Pistacia",
    "pistachia" = "Pistacia"
  )
  species <- c(
    "anthyllis" = "citisoides",
    "dorycnium" = "pentaphyllum",
    "pistacia" = "lentiscus",
    "pistachia" = "lentiscus"
  )

  if (type == "genra") {
    return(genra)
  } else if (type == "short") {
    out <- paste0(
      map_chr(genra, ~substr(.x, 1, 1)),
      ". ",
      species)
    names(out) <- names(genra)
    return(out)
  } else {
    out <- paste0(genra, " ", species)
    names(out) <- names(genra)
    return(out)
  }

}

rand_term_replacement <- function() {
  c(
    epsilon = "Residual",
    IDtp = "Plot effect",
    IDtpl = "Site effect",
    IDtl = "Site effect"
  )
}

com_replacement <- function() {
  c("Mono" = "Monospecific", "Poly" = "Diverse")
}

ms_replacement <- function() {
  c("Open" = "Open", "Patch" = "Nurse patch")
}

water_replacement <- function() {
  c(
    "No Watered" = "Not Watered",
    "Watered" = "Watered"
  )
}

div_eff_replacement <- function() {

  c(
    "net" = "Net",
    "complementarity" = "Complementarity",
    "selection" = "Selection"
  )

}
