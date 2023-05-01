
term_replacement <- function() {
  c(
    "duration_m" = "Time",
    "comPoly" = "Diversity",
    "msPatch" = "Patch",
    "wateringWatered" = "Watering",
    "speciesdorycnium" = "Dorycnium",
    "speciespistachia" = "Pistacia",
    "\\bter\\b" = "Terrace",
    ":" = " x\n"
  )
}

response_replacement <- function() {
  c(
    "d" = "Basal diameter",
    "survival" = "Survival",
    "log_d" = "Basal diameter",
    "h" = "Vegetative height",
    "hm" = "Max vegetative height",
    "bm" = "Total biomass",
    "ldmc" = "Leaf Dry Matter Content",
    "sla" = "Specific Leaf Area"
  )
}

species_replacement <- function(type = "short") {
  genra <- c(
    "anthyllis" = "Anthyllis",
    "dorycnium" = "Dorycnium",
    "pistacia" = "Pistacia"
  )
  species <- c(
    "anthyllis" = "citisoides",
    "dorycnium" = "pentaphyllum",
    "pistacia" = "lentiscus"
  )

  if (type == "genra") {
    return(genra)
  } else if (type == "short") {
    out <- paste0(
      map_chr(genra, ~substr(.x, 1, 1)),
      ". ",
      species)
    return(out)
  } else {
    out <- paste0(genra, " ", species)
    return(out)
  }

}
